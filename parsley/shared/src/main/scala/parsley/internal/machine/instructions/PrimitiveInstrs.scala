/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.state.Ref
import parsley.token.errors.LabelConfig

import parsley.internal.errors.ExpectDesc
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.errors.NoError
import scala.collection.mutable

import parsley.internal.machine.RecoveryState

private [internal] final class Satisfies(f: Char => Boolean, expected: Iterable[ExpectDesc]) extends Instr {
    def this(f: Char => Boolean, expected: LabelConfig) = this(f, expected.asExpectDescs)
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if (ctx.moreInput && f(ctx.peekChar)) ctx.pushAndContinue(ctx.consumeChar())
        else ctx.expectedFail(expected, unexpectedWidth = 1)
    }
    // $COVERAGE-OFF$
    override def toString: String = "Sat(?)"
    // $COVERAGE-ON$
}

private [internal] object RestoreAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        // Pop input off head then fail to next handler
        ctx.restoreState()
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = "RestoreAndFail"
    // $COVERAGE-ON$
}

// this pops handler, clears errors and pops errors
private [internal] object PopHandlerAndErrorsAndClearErrors extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.restoreState()
        assert(!ctx.errorState.isLive, "Cannot restore old state and errors with live error")
        ctx.errorState = NoError
        ctx.popAndMergeErrors()
        ctx.handlers = ctx.handlers.tail
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopHandlerAndErrorsAndRestoreErrors"
    // $COVERAGE-ON$
}

private [internal] object PopStateAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        ctx.states = ctx.states.tail
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopStateAndFail"
    // $COVERAGE-ON$
}

private [internal] object PopStateAndErrorsAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.popAndMergeErrors()
        ctx.handlers = ctx.handlers.tail
        ctx.states = ctx.states.tail
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopStateAndErrorsAndFail"
    // $COVERAGE-ON$
}

private [internal] object PopErrorsAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.popAndMergeErrors()
        ctx.handlers = ctx.handlers.tail
        ctx.states = ctx.states.tail

        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = "PopStateRestoreHintsAndFail"
    // $COVERAGE-ON$
}

// Position Extractors
private [internal] object Line extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushAndContinue(ctx.line)
    }
    // $COVERAGE-OFF$
    override def toString: String = "Line"
    // $COVERAGE-ON$
}

private [internal] object Col extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushAndContinue(ctx.col)
    }
    // $COVERAGE-OFF$
    override def toString: String = "Col"
    // $COVERAGE-ON$
}

private [internal] object Offset extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushAndContinue(ctx.offset)
    }
    // $COVERAGE-OFF$
    override def toString: String = "Offset"
    // $COVERAGE-ON$
}

// Register-Manipulators
private [internal] final class Get(reg: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.pushAndContinue(ctx.regs(reg))
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Get(r$reg)"
    // $COVERAGE-ON$
}

private [internal] final class Put(reg: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.writeReg(reg, ctx.stack.upop())
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Put(r$reg)"
    // $COVERAGE-ON$
}

private [internal] final class PutAndFail(reg: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        ctx.writeReg(reg, ctx.stack.upeek)
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"PutAndFail(r$reg)"
    // $COVERAGE-ON$
}

private [internal] object Span extends Instr {
    override def apply(ctx: Context): Unit = {
        // this uses the state stack because post #132 we will need a save point to obtain the start of the input
        ensureRegularInstruction(ctx)
        val startOffset = ctx.states.offset
        ctx.states = ctx.states.tail
        ctx.handlers = ctx.handlers.tail
        ctx.pushAndContinue(ctx.input.substring(startOffset, ctx.offset))
    }
    // $COVERAGE-OFF$
    override def toString: String = "Span"
    // $COVERAGE-ON$
}

// This instruction holds mutate state, but it is safe to do so, because it's always the first instruction of a DynCall.
private [parsley] final class CalleeSave(var label: Int, localRegs: Set[Ref[_]], reqSize: Int, slots: List[(Int, Int)])
    extends InstrWithLabel {
    // this filters out the slots to ensure we only do callee-save on registers that might exist in the parent
    def this(label: Int, localRefs: Set[Ref[_]], reqSize: Int, slots: List[Int], numRegsInContext: Int) =
        this(label, localRefs, reqSize, slots.takeWhile(_ < numRegsInContext).zipWithIndex)

    var saveArray =  new Array[AnyRef](slots.length)
    private var inUse = false
    private var oldRegs: Array[AnyRef] = null



    private var validRecoveryPoints: List[RecoveryState] = List.empty
    // old regs and save array
    private val oldRecoveryPoints: mutable.Map[RecoveryState, (Array[AnyRef], Array[AnyRef])] = mutable.Map.empty


    private [machine] def addRecoveryPoint(point: RecoveryState) = {
        assert(inUse, "CalleeSaves should only be in recovery instruction stack if applied")
        this.validRecoveryPoints = point :: this.validRecoveryPoints;

    }

    private [machine] def recoverTo(point: RecoveryState) = {

        val alreadyValid = this.validRecoveryPoints.contains(point)
        if(inUse && !this.validRecoveryPoints.isEmpty) flushValidRecoveryPoints()

        this.inUse = true
        
        if(!alreadyValid) {
            this.oldRecoveryPoints.get(point) match {
                case None => throw new IllegalStateException("Cannot recover to a point not recorded")
                case Some((restoreOldRegs, restoreSaveArray)) => {
                    this.oldRegs = restoreOldRegs
                    this.saveArray = restoreSaveArray
                } 
            }
        }
    }

    private def flushValidRecoveryPoints() = {

        val parentRegArray = oldRegs
        val saveArrayCopy = java.util.Arrays.copyOf(saveArray, saveArray.length)

        this.oldRecoveryPoints.addAll(this.validRecoveryPoints.map(_ -> (parentRegArray, saveArrayCopy)))
        this.validRecoveryPoints = List.empty
    }

    private def save(ctx: Context): Unit = {
        for ((slot, idx) <- slots) {
            saveArray(idx) = ctx.regs(slot)
            ctx.regs(slot) = null
        }
        // If this is known to increase the size of the register pool, then we need to keep the old array to the side
        if (reqSize > ctx.regs.size) {
            oldRegs = ctx.regs
            ctx.regs = java.util.Arrays.copyOf(oldRegs, reqSize)
        }
    }

    private def restore(ctx: Context): Unit = {
        // When we leave this register scope, we need to keep a record of the registers 
        // as if we recover, this CalleeSave won't be in the right state
        flushValidRecoveryPoints()

        // Put the values from maintained registers back in the original regs
        if (oldRegs != null) {
            java.lang.System.arraycopy(ctx.regs, 0, oldRegs, 0, oldRegs.size)
            ctx.regs = oldRegs
            oldRegs = null
        }
        for ((slot, idx) <- slots) {
            ctx.regs(slot) = saveArray(idx)
            saveArray(idx) = null
        }
        // This is the only way to get them reallocated on the next invocation
        // FIXME: I think this isn't thread-safe, because two flatMaps can simulataneously reallocate?
        localRegs.foreach(_.deallocate())
    }

    private def continue(ctx: Context): Unit = {
        ctx.handlers = ctx.handlers.tail
        if (ctx.good) ctx.pc = label
        else ctx.fail()
    }

    override def apply(ctx: Context): Unit = {
        // Second-entry, callee-restore and either jump or fail
        if (inUse) {
            restore(ctx)
            inUse = false
            continue(ctx)
        }
        // Entry for the first time, register as a handle, callee-save and inc
        else {
            ensureRegularInstruction(ctx)
            save(ctx)
            inUse = true
            ctx.pushHandler(ctx.pc)
            ctx.inc()
        }
    }

    // $COVERAGE-OFF$
    override def toString: String = s"CalleeSave($label, newSz = $reqSize, slotsToSave = $slots)"
    // $COVERAGE-ON$
}
