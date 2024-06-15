/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine

import scala.annotation.tailrec

//import parsley.{Failure, Result, Success} // not sure why this fails scalacheck, but I guess we'll leave it until I can submit a bug report
import parsley.Failure
import parsley.Result
import parsley.Success
import parsley.XAssert._
import parsley.errors.ErrorBuilder

import parsley.internal.diagnostics.RegisterOutOfBoundsException
import parsley.internal.errors.{CaretWidth, ExpectItem, LineBuilder, UnexpectDesc}
import parsley.internal.machine.errors.{ClassicFancyError, DefuncError, DefuncHints, EmptyHints,
                                        ErrorItemBuilder, ExpectedError, ExpectedErrorWithReason, UnexpectedError, EmptyError}

import instructions.Instr
import stacks.{ArrayStack, CallStack, ErrorStack, ErrorStateStack, HandlerStack, RecoveryStack, Stack, StateStack}, Stack.StackExt
import parsley.internal.machine.errors.ErrorState
import parsley.internal.machine.errors.NoError
import parsley.internal.machine.errors.LiveError
import parsley.internal.machine.errors.AccumulatorError
import parsley.Recovered
import parsley.MultiFailure
import parsley.internal.machine.instructions.CalleeSave
import parsley.internal.machine.errors.NoError.isEmpty

private [parsley] final class Context(private [machine] var instrs: Array[Instr],
                                      private [machine] val input: String,
                                      numRegs: Int,
                                      private val sourceFile: Option[String]) {

    private val debug = false

    private [machine] var reasonCount = 0
    private [machine] var amendCount = 0
    private [machine] var labelCount = 0
    private [machine] var entrenchCount = 0
    private [machine] var dislodgeCount = 0


    private val positionTracker = new PositionTracker(input)
    

    private val UnitError = new EmptyError(0, 0) 
    /** This is the operand stack, where results go to live  */
    private [machine] var stack: ArrayStack[Any] = new ArrayStack()
    /** Current offset into the input */
    private [machine] var offset: Int = 0
    /** The length of the input, stored for whatever reason */
    private [machine] val inputsz: Int = input.length
    /** Call stack consisting of Frames that track the return position and the old instructions */
    private var calls: CallStack = Stack.empty
    /** State stack consisting of offsets and positions that can be rolled back */
    private [machine] var states: StateStack = Stack.empty
    /** Stack consisting of offsets at previous checkpoints, which may query to test for consumed input */
    /** Current operational status of the machine */
    private [machine] var good: Boolean = true
    private [machine] var running: Boolean = true
    /** Stack of handlers, which track the call depth, program counter and stack size of error handlers */
    private [machine] var handlers: HandlerStack = Stack.empty

    private [machine] var check: Int = 0

    /** Current offset into program instruction buffer */
    private [machine] var pc: Int = 0
    /** Current line number */
    private [machine] def line: Int = positionTracker.getPos(offset)._1
    /** Current column number */
    private [machine] def col: Int = positionTracker.getPos(offset)._2
    /** State held by the registers, AnyRef to allow for `null` */
    private [machine] var regs: Array[AnyRef] = new Array[AnyRef](numRegs)
    /** Amount of indentation to apply to debug combinators output */
    private [machine] var debuglvl: Int = 0

    private [machine] var errorState: DefuncError = new EmptyError(0, 0)
    private [machine] var isLiveError: Boolean = false
    private [machine] var isEmptyError: Boolean = true

    // In Recovery
    /**
      * The error which caused the beginning of this recovery
      */
    private [machine] var parkedError: Option[DefuncError] = None
    /**
      * If we have recovery within recovery, we need to disregard these errors 
      * but keep the state so we record just the depth and when it hits 0, parked error is 
      * the original error to display to the user
      */
    private [machine] var recoveryDepth: Int = 0


    // Recovery Contrl Flow


    private [machine] var recoveredErrors: List[DefuncError] = List.empty
    // This only gets populated once we've started trying other options than the best failure
    private [machine] var bestFailureSnapshot: Option[List[DefuncError]] = None
    private [machine] var recoveryStack: RecoveryStack = new RecoveryStack(List.empty, false, 0, Stack.empty)
    private [machine] var forceRecovery: Boolean = false



    /**
      * Our errors are now ready to be merged back into the parent scope
      * If we have a live error, apply accumulator, pop the stack and apply the accumulator from there 
      * This puts us in a state of everything up to date with a live error in the parent
      * 
      * If no live error, merge the accumulators
      */
    private [machine] def popAndMergeErrors(): Unit = {
        // Normal errors
        // val (stackError, stackRecoveredErrors) = this.errorStack.pop();
        val stackError = this.handlers.error

        if(!this.handlers.isEmptyError) {

            // If we have some error from the stack, we let the current state decide how to merge it 
        // and if no current state we just default to the stack state
            this.errorState = this.errorState.merge(stackError)
            this.isEmptyError = false
        }

        if(!this.handlers.recoveredErrors.isEmpty) {
            this.recoveredErrors = this.recoveredErrors ++ this.handlers.recoveredErrors
        }
    }



    private [machine] def addHints(expecteds: Set[ExpectItem], unexpectedWidth: Int) = {
        assume(expecteds.nonEmpty, "hints must always be non-empty")
        val newError = new ExpectedError(this.offset, expecteds, unexpectedWidth)

        assert(!isLiveError, "Cannot add hints with a live error")
        this.errorState = this.errorState.merge(newError)
    }


    // Error Recovery

    private [machine] def setupRecovery() = {
        if(debug) println("setting up recovery")
        
        assume(isLiveError, "Cannot move error to recovery if not live")
        if(this.recoveryDepth == 0) {
            this.parkedError = Some(this.errorState)
        }

        this.recoveryDepth += 1
        // Regardless of whether we keep the error we need to clear it when we begin recovering
        this.errorState = UnitError
        this.isEmptyError = true
        this.isLiveError = false
    }

    private [machine] def succeedRecovery() = {
        if(debug) println("Succeeding recovery")

        assume(this.parkedError.isDefined, "Cannot commit a recovered error if no recovered errors in flight");
        assume(this.recoveryDepth > 0, "need at least one recovery scope")
        this.recoveryDepth -= 1        
        // we only want errors if at base recovery combinator 
        //otherwise we are accumulating errors in a recovery state
        if(this.recoveryDepth == 0) {
            this.recoveredErrors = this.parkedError.get :: this.recoveredErrors
            this.parkedError = None
        }
    }

    /**
      * When recovery fails, we want to keep the message from the original fail not recovery
      * Only handled in case of base recovery state for efficiency
      */

    private [machine] def failRecovery() = {
        if(debug) println("failing recovery")

        setupRecoveryStack()
        val canRecover = !this.recoveryStack.recoveryPoints.isEmpty
        if(canRecover) this.recoverToFurthestPoint()
        else {
            if(bestFailureSnapshot.isDefined) {
                this.good = false
                this.running = false
            } else {
                restoreAfterFailedRecovery()   
            }
        }
    }

    private [machine] def restoreAfterFailedRecovery(): Unit = {
        assert(isLiveError, "Cannot replace a recovery error unless there is a live error")
        this.recoveryDepth -= 1
        if(this.recoveryDepth == 0) {
            this.errorState = this.parkedError.getOrElse(new EmptyError(0,0))
            this.isLiveError = true
            this.parkedError = None
        }

        this.popAndMergeErrors()
        this.handlers = this.handlers.tail
        this.restoreState()
        this.fail()
    }


    
    def insertOrdered[A](elem: A, list: List[A])(compare: (A, A) => Boolean): List[A] = {
        list.foldRight(List(elem)) { (current, acc) =>
          if (compare(current, elem)) current :: acc
          else elem :: current :: acc.tail
        }
    }

    private [machine] def pushRecoveryPoint(): Unit = {

        assume(isLiveError, "Cannot push recovery point if not live error")

        val oldRegs = java.util.Arrays.copyOf(this.regs, this.regs.length)
        
        val recoveryPoint: RecoveryState = new RecoveryState(
            this.handlers, this.stack.clone(), 
            this.calls, this.states, oldRegs, this.instrs,
            this.errorState, this.recoveredErrors,
            this.parkedError, this.recoveryDepth,
            this.pc, this.offset)
            
        traverseCalleSaves(this.calls, _.addRecoveryPoint(recoveryPoint))
        applyToCalleeSaves(this.instrs, _.addRecoveryPoint(recoveryPoint))
        // This puts deeper offsets first and also later pushed points first (>=)
            
        this.recoveryStack.recoveryPoints = insertOrdered[RecoveryState](recoveryPoint, this.recoveryStack.recoveryPoints)(_.offset >= _.offset)
    }


    private def applyToCalleeSaves(instrs: Array[Instr], action: CalleeSave => Unit): Unit = {
        instrs match {
            case Array(cSave: CalleeSave, _*) => action(cSave)
            case _ => 
        }
    }
    @tailrec
    private def traverseCalleSaves(callStack: CallStack, action: CalleeSave => Unit): Unit = {

        if(callStack.isEmpty) return 
        applyToCalleeSaves(callStack.instrs, action)
        traverseCalleSaves(callStack.tail, action)
    }



    private def restoreContext(recoveryPoint: RecoveryState) = {
        this.errorState = recoveryPoint.currentError
        this.isLiveError = true

        this.recoveredErrors = recoveryPoint.recoveredErrors


        this.handlers = recoveryPoint.handlers
        this.stack = recoveryPoint.data
        this.states = recoveryPoint.states
        this.calls = recoveryPoint.callStack

        this.regs = recoveryPoint.regs
        this.instrs = recoveryPoint.instrs

        this.offset = recoveryPoint.offset
        this.pc = recoveryPoint.pc

        this.parkedError = recoveryPoint.parkedError
        this.recoveryDepth = recoveryPoint.recoveryDepth

        traverseCalleSaves(this.calls, _.recoverTo(recoveryPoint))
        applyToCalleeSaves(this.instrs, _.recoverTo(recoveryPoint))
    }

    private [machine] def recoverToFurthestPoint(): Unit = {
        if(debug) println("recovering to furthest point")

        assert(!good, "Recovery not possible if context not in a recovery state")
        if(this.recoveryStack.recoveryOffset > this.offset) {
            if(this.recoveryStack.tail.isEmpty) {
                this.running = false
            } else {
                this.recoveryStack = this.recoveryStack.tail
                // Called recursively incase multiple invalid recovery points
                this.recoverToFurthestPoint()
            }
        } else {
            val recoveryPoint = this.recoveryStack.recoveryPoints.head
            if(debug) {

                println("Beginning Recovery")
                println(this.recoveryStack.recoveryPoints.mkString)
                println(s"At offset ${recoveryPoint.offset}")
                println("____")
            }
    
    
            this.restoreContext(recoveryPoint);
            this.forceRecovery = true

            this.recoveryStack.recoveryPoints = this.recoveryStack.recoveryPoints.tail
            this.recoveryStack.recoveryAttempted = true
            this.recoveryStack = new RecoveryStack(
                List.empty, false,
                recoveryPoint.offset, 
                this.recoveryStack)
        }
    }

    // End: Error Recovery
    private [machine] def updateCheckOffset() = {
        this.check = this.offset
    }


    // $COVERAGE-OFF$
    private [machine] def pretty: String = {
        s"""[
           |  stack     = [${stack.mkString(", ")}]
           |  instrs    = ${instrs.toList.mkString("; ")}
           |  input     = ${input.drop(offset)}
           |  pos       = ($line, $col)
           |  status    = $status
           |  pc        = $pc
           |  rets      = ${calls.mkString(", ")}
           |  handlers  = ${handlers.mkString(", ")}
           |  recstates = ${states.mkString(", ")}
           |  registers = ${regs.zipWithIndex.map{case (r, i) => s"r$i = $r"}.toList.mkString("\n              ")}
           |]""".stripMargin
    }
    // $COVERAGE-ON$

    private [parsley] def run[Err: ErrorBuilder, A](): Result[Err, A] = {
        // if(debug) println(this.pretty)
        try go[Err, A]()
        catch {
            // additional diagnostic checks
            // $COVERAGE-OFF$
            case RegisterOutOfBoundsException(err) => throw err // scalastyle:ignore throw
            // $COVERAGE-ON$
        }
    }
    @tailrec private def go[Err: ErrorBuilder, A](): Result[Err, A] = {
        //println(pretty)
        if (running) { // this is the likeliest branch, so should be executed with fewest comparisons
            if(debug) {

                print("Running New Inst: ")
                println(instrs(pc))
                println(this.errorState)

                // println(this.errorStack.mkString(", "))
                println(s"[${this.recoveredErrors.mkString(", ")}]")
                println("____")
            }
            instrs(pc)(this)
            go[Err, A]()
        }
        else if (good) {
            assert(stack.size == 1, s"stack must end a parse with exactly one item, it has ${stack.size}")
            assert(calls.isEmpty, "there must be no more calls to unwind on end of parser")
            assert(handlers.isEmpty, "there must be no more handlers on end of parse")
            assert(states.isEmpty, "there must be no residual states left at end of parse")
            // assert(errs.isEmpty, "there should be no parse errors remaining at end of parse")

            if(recoveredErrors.isEmpty) {
                Success(stack.peek[A])
            } else {
                val errs = recoveredErrors.map(e => {
                    val parseErr = e.asParseError
                    val (line, col) = positionTracker.getPos(parseErr.offset)
                    parseErr.col = col
                    parseErr.line = line
                    parseErr.format(sourceFile)
                })
                Recovered(stack.peek[A], recoveredErrors = errs)
            }
        }
        else {
            assert(isLiveError, "Error must be live during failure")
           

            val error = this.errorState
            // If we've failed and we have a best failure snapshot 
            // it means we've tried other paths so need to go back to those errors
            val relevantErrors = bestFailureSnapshot match {
                case Some(value) => value
                case None => error :: recoveredErrors
            }

            val parserErrors = relevantErrors.map(e => {
                val parseErr = e.asParseError
                val (line, col) = positionTracker.getPos(parseErr.offset)
                parseErr.col = col
                parseErr.line = line
                parseErr.format(sourceFile)
            })
            
            val result = parserErrors match {
                case one :: Nil => Failure(one)
                case many => MultiFailure(many)
            }
            result
        }
    }



    private [machine] def makeErrorAccumulator() = {
        assert(!this.isEmptyError, "Cannot make error accumulator if not defined")
        this.isLiveError = false
    }



    private [machine] def call(newInstrs: Array[Instr]): Unit = {
        call(0)
        instrs = newInstrs
    }

    private [machine] def call(at: Int): Unit = {
        calls = new CallStack(pc + 1, instrs, at, calls)
        pc = at
    }

    private [machine] def ret(): Unit = {
        assert(calls != null, "cannot return when no calls are made")
        instrs = calls.instrs
        pc = calls.ret
        calls = calls.tail
    }

    private [machine] def catchNoConsumed(check: Int)(handler: =>Unit): Unit = {
        assert(!good, "catching can only be performed in a handler")
        if (offset != check) {
            handlers = handlers.tail
            fail()
        }
        else {
            good = true
            handler
        }
    }

    
    private [machine] def clearError(): Unit = {
        this.isEmptyError = true
        this.isLiveError = false
        this.errorState = UnitError
    }
    /* Eagerly apply scope accumulator to the error message when we push a new error */

    private [machine] def pushError(err: DefuncError): Unit = {

        this.errorState = this.errorState.merge(err)
        this.isLiveError = true
        this.isEmptyError = false
    }
    
    // TODO (Dan) this isn't working properly - need to see if its an amend issue?
    private [machine] def pushAccumulatorError(err: DefuncError, offset: Int): Unit = {
        assert(!isLiveError, "cannot push hints if we have a live error")

    
        this.isEmptyError = false
        this.errorState = this.errorState.merge(err)
    }

    private [machine] def failWithMessage(caretWidth: CaretWidth, msgs: String*): Unit = {
        this.fail(new ClassicFancyError(offset, caretWidth, msgs: _*))
    }
    private [machine] def unexpectedFail(expected: Iterable[ExpectItem], unexpected: UnexpectDesc): Unit = {
        this.fail(new UnexpectedError(offset, expected, unexpected))
    }
    private [machine] def expectedFail(expected: Iterable[ExpectItem], unexpectedWidth: Int): Unit = {
        this.fail(new ExpectedError(offset, expected, unexpectedWidth))
    }
    private [machine] def expectedFailWithReason(expected: Iterable[ExpectItem], reason: String, unexpectedWidth: Int): Unit = {
        this.fail(new ExpectedErrorWithReason(offset, expected, reason, unexpectedWidth))
    }
    private [machine] def expectedFailWithReason(expected: Iterable[ExpectItem], reason: Option[String], unexpectedWidth: Int): Unit = {
        if (reason.isEmpty) this.expectedFail(expected, unexpectedWidth)
        else this.expectedFailWithReason(expected, reason.get, unexpectedWidth)
    }

    private [machine] def fail(error: DefuncError): Unit = {
        good = false
        this.pushError(error)
        this.fail()
    }

    private def setupRecoveryStack(): Unit = {
        val recoveryPoints = this.recoveryStack.recoveryPoints
        while(!this.recoveryStack.tail.isEmpty && recoveryPoints.isEmpty ) {            
            this.recoveryStack = this.recoveryStack.tail
        }
    }

    private [machine] def fail(): Unit = {
        assert(!good, "fail() may only be called in a failing context, use `fail(err)` or set `good = false`")
        if (!handlers.isEmpty) {
            val handler = handlers
            instrs = handler.instrs
            calls = handler.calls
            pc = handler.pc
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
        } else {

            // Very horrible why - but this is the best attempt failure
            if(!this.recoveryStack.recoveryAttempted && !this.recoveryStack.tail.isEmpty && this.bestFailureSnapshot.isEmpty) {
                if(debug) println("saving best snapshot")
                this.bestFailureSnapshot = Some(this.errorState :: this.recoveredErrors)
            }

            setupRecoveryStack()
            val canRecover = !this.recoveryStack.recoveryPoints.isEmpty
            if(canRecover) this.recoverToFurthestPoint()
            else this.running = false
        }
    }

    private [machine] def pushAndContinue(x: Any) = {
        stack.push(x)
        inc()
    }
    private [machine] def unsafePushAndContinue(x: Any) = {
        stack.upush(x)
        inc()
    }
    private [machine] def exchangeAndContinue(x: Any) = {
        stack.exchange(x)
        inc()
    }
    private [machine] def inc(): Unit = pc += 1
    private [machine] def peekChar: Char = input.charAt(offset)
    private [machine] def peekChar(lookAhead: Int): Char = input.charAt(offset + lookAhead)
    private [machine] def moreInput: Boolean = offset < inputsz
    private [machine] def moreInput(n: Int): Boolean = offset + (n - 1) < inputsz

    private [machine] def consumeChar(): Char = {
        val c = peekChar
        offset += 1
        c
    }

    private [machine] def consumeChar_(): Unit = {
        offset += 1
    }
    
    private [machine] def fastConsumeSupplementaryChar(): Unit = {
        assert(this.peekChar.isHighSurrogate, "must have a high surrogate to consume supplementary")
        // not going to be a tab or newline
        offset += 2
    }
    private [machine] def fastUncheckedConsumeChars(n: Int): Unit = {
        offset += n
    }

    private [machine] def pushHandler(label: Int): Unit = {
        // we don't include error unless necessary to help GC
        handlers = new HandlerStack(calls, instrs, UnitError, true, List.empty, label, stack.usize, offset, check, handlers)
    }

    private [machine] def pushHandlerAndErrors(label: Int): Unit = {
        // we don't include error unless necessary to help GC
        handlers = new HandlerStack(calls, instrs, errorState, false, this.recoveredErrors, label, stack.usize, offset, check, handlers)
        errorState = UnitError
        isEmptyError = true
        isLiveError = false
        recoveredErrors = List.empty
    }


    /**
      * Updates the head of the handler stack immutably
      * although this may look inefficient, it only occurs within filtering and means
      * handler positions don't need to be udpated regularly
      * @param label new instruction label
      */
    private [machine] def setHandlerPC(label: Int): Unit = {
        handlers = new HandlerStack(
            handlers.calls,
            handlers.instrs,
            handlers.error,
            handlers.isEmptyError,
            handlers.recoveredErrors,
            label,
            handlers.stacksz,
            handlers.offset,
            handlers.prevCheck,
            handlers.tail
        )
    }

    private [machine] def saveState(): Unit = states = new StateStack(offset, line, col, states)
    private [machine] def restoreState(): Unit = {
        val state = states
        states = states.tail
        offset = state.offset
    }
    private [machine] def writeReg(reg: Int, x: Any): Unit = {
        regs(reg) = x.asInstanceOf[AnyRef]
    }

    private [machine] def status: Status = {
        if (running) if (good) Good else Recover
        else if (good) Finished else Failed
    }

    private implicit val lineBuilder: LineBuilder = new LineBuilder {
        def nearestNewlineBefore(off: Int): Option[Int] = {
            if (off < 0) None
            else Some {
                val idx = Context.this.input.lastIndexOf('\n', off-1)
                if (idx == -1) 0 else idx + 1
            }
        }
        def nearestNewlineAfter(off: Int): Option[Int] = {
            if (off > Context.this.inputsz) None
            else Some {
                val idx = Context.this.input.indexOf('\n', off)
                if (idx == -1) Context.this.inputsz else idx
            }
        }
        def segmentBetween(start: Int, end: Int): String = {
            Context.this.input.substring(start, end)
        }
    }

    private [machine] implicit val errorItemBuilder: ErrorItemBuilder = new ErrorItemBuilder {
        def inRange(offset: Int): Boolean = offset < Context.this.inputsz
        def codePointAt(offset: Int): Int = Context.this.input.codePointAt(offset)
        //def substring(offset: Int, size: Int): String = Context.this.input.substring(offset, Math.min(offset + size, Context.this.inputsz))
        def iterableFrom(offset: Int): IndexedSeq[Char] = Context.this.input.substring(offset)
    }
}
