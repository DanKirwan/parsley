/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.instructions

import parsley.internal.errors.{CaretWidth, RigidCaret, UnexpectDesc}
import parsley.internal.machine.Context
import parsley.internal.machine.XAssert._
import parsley.internal.machine.errors.EmptyError
import parsley.internal.machine.stacks.ErrorStack
import parsley.internal.machine.stacks.Stack.StackExt
import parsley.internal.machine.errors.LiveError
import parsley.internal.machine.errors.NoError



private [internal] abstract class ScopeExit(val isErrorScope: Boolean, val isStateScope: Boolean) extends Instr {

    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        cleanup(ctx)
        
        ctx.handlers = ctx.handlers.tail
        if(isStateScope) ctx.states = ctx.states.tail
        if(isErrorScope) ctx.popAndMergeErrors()

        ctx.inc()
    }

    def cleanup(ctx: Context): Unit
}
/**
  * This is used at the beginning of a new choice option 
  * It will merge the existing errors in live into the choice accumulator
  *
  */
private [internal] object ClearLiveError extends Instr {

    override def apply(ctx: Context): Unit = {
        ctx.makeErrorAccumulator()
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ClearLiveError";
    // $COVERAGE-ON$

}


private [internal] final class RelabelErrorAndFail(labels: Iterable[String]) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)

        // TODO (Dan) replace with context calls 
        assert(ctx.errorState.isLive, "Cannot relabel if we don't have a live error");

        ctx.errorState = ctx.errorState.map(e => e.label(labels, ctx.handlers.check))
        ctx.popAndMergeErrors()
        ctx.handlers = ctx.handlers.tail
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"RelabelErrorAndFail($labels)"
    // $COVERAGE-ON$
}


private [internal] class RelabelExit(labels: Iterable[String]) extends ScopeExit(true, false) {
    private val isHide = labels.isEmpty
    override def cleanup(ctx: Context): Unit = {
        assert(!ctx.errorState.isLive, "cannot label accumulator if in live state")
        // Here we may have no hints in which case we just ignore relabelling them
        if(isHide) ctx.errorState = NoError
        else if(ctx.offset == ctx.handlers.check) ctx.errorState = ctx.errorState.map(e => e.label(labels, ctx.offset))

        

        ctx.recoveredErrors = ctx.recoveredErrors.map(e => e.label(labels, ctx.handlers.check))
    }
    // $COVERAGE-OFF$
    override def toString: String = "RelabelExit"
    // $COVERAGE-ON$
}

// FIXME: Gigaparsec points out the hints aren't being used here, I believe they should be!
private [internal] object HideErrorAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        assert(ctx.errorState.isLive, "Cannot hide if we don't have a live error");
        ctx.errorState = LiveError(new EmptyError(ctx.offset, ctx.line, ctx.col, unexpectedWidth = 0))
        ctx.popAndMergeErrors()
        ctx.handlers = ctx.handlers.tail
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = "HideErrorAndFail"
    // $COVERAGE-ON$
}



private [internal] object HideExit extends ScopeExit(true, false) {

    override def cleanup(ctx: Context): Unit = {
        assert(!ctx.errorState.isLive, "Cannot hide accumulator errors if we have an existing error")
        // TODO (Dan) confirm we want to set error state here and set list empty?
        ctx.errorState = NoError
        ctx.recoveredErrors = List.empty
    }
    // $COVERAGE-OFF$
    override def toString: String = "HideExit"
    // $COVERAGE-ON$
}


private [internal] object ErrorToHints extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        // TODO (Dan) why does this remove a handler?
        ctx.handlers = ctx.handlers.tail

        // ctx.addErrorToHintsAndPop()
        
        
        // if(ctx.liveError.isDefined) {
        //     // TODO (Dan) this if shouldn't be here - figure out how to change instruction set semantics
        //     assert(ctx.liveError.isDefined, "Cannot send error to hints if no error has been thrown")
        //     ctx.errorToAccumulator();
            
        // }
        ctx.inc()

    }

    // $COVERAGE-OFF$
    override def toString: String = "ErrorToHints"
    // $COVERAGE-ON$
}

private [internal] object MergeErrorsAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        // TODO (Dan) why does this remove a handler?
        ctx.handlers = ctx.handlers.tail   
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "MergeErrorsAndFail"
    // $COVERAGE-ON$
}

private [internal] class ApplyReasonAndFail(reason: String) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)

        assert(ctx.errorState.isLive, "Cannot apply reason if we don't have a live error");
        ctx.errorState = ctx.errorState.map(e => e.withReason(reason, ctx.handlers.check))
        ctx.popAndMergeErrors()
        // Why does this remove a handler?
        ctx.handlers = ctx.handlers.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = s"ApplyReasonAndFail($reason)"
    // $COVERAGE-ON$
}


private [internal] class ReasonExit(reason: String) extends ScopeExit(true, false) {

    override def cleanup(ctx: Context): Unit = {
        ctx.recoveredErrors = ctx.recoveredErrors.map(e => e.withReason(reason, ctx.handlers.check))
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ReasonExit($reason)"
    // $COVERAGE-ON$

}



private [internal] class AmendAndFail private (partial: Boolean) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        assert(ctx.errorState.isLive, "Cannot amend if we don't have a live error");

        ctx.errorState = ctx.errorState.map(e => e.amend(partial, ctx.states.offset, ctx.states.line, ctx.states.col))
        ctx.popAndMergeErrors()
        ctx.states = ctx.states.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "AmendAndFail"
    // $COVERAGE-ON$
}
private [internal] object AmendAndFail {
    private [this] val partial = new AmendAndFail(partial = true)
    private [this] val full = new AmendAndFail(partial = false)
    def apply(partial: Boolean): AmendAndFail = if (partial) this.partial else this.full
}

private [internal] class AmendExit(partial: Boolean) extends ScopeExit(true, true) {

    override def cleanup(ctx: Context): Unit = {
        ctx.recoveredErrors = ctx.recoveredErrors.map(e => e.amend(partial, ctx.states.offset, ctx.states.line, ctx.states.col))
    }
    
    // $COVERAGE-OFF$
    override def toString: String = s"AmendExit"
    // $COVERAGE-ON$

}


private [internal] object EntrenchAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        // TODO (Dan) do we need to do anything with accumulator errors
        assert(ctx.errorState.isLive, "Cannot entrench l if we don't have a live error");

        ctx.errorState = ctx.errorState.map(e => e.entrench)

        ctx.popAndMergeErrors()
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "EntrenchAndFail"
    // $COVERAGE-ON$
}


private [internal] object EntrenchExit extends ScopeExit(true, false) {
    override def cleanup(ctx: Context): Unit = {
        ctx.recoveredErrors = ctx.recoveredErrors.map(x => x.entrench)
    }

    
    // $COVERAGE-OFF$
    override def toString: String = "EntrenchExit"
    // $COVERAGE-ON$
  }

private [internal] class DislodgeAndFail(n: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        // TODO (Dan) do we need to do anything with accumulator errors
        assert(ctx.errorState.isLive, "Cannot dislodge if we don't have a live error");
        ctx.errorState = ctx.errorState.map(e => e.dislodge(n))
        ctx.popAndMergeErrors()
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = s"DislodgeAndFail($n)"
    // $COVERAGE-ON$
}


private [internal] class DislodgeExit(n: Int) extends ScopeExit(true, false) {
    override def cleanup(ctx: Context): Unit = {
        ctx.recoveredErrors = ctx.recoveredErrors.map(x => x.dislodge(n))
    }

    
    // $COVERAGE-OFF$
    override def toString: String = "DislodgeExit"
    // $COVERAGE-ON$
}

private [internal] object SetLexicalAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        assert(ctx.errorState.isLive, "Cannot set lexical if we don't have a live error");
        ctx.errorState = ctx.errorState.map(x => x.markAsLexical(ctx.handlers.check))
        ctx.popAndMergeErrors()
        ctx.handlers = ctx.handlers.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "SetLexicalAndFail"
    // $COVERAGE-ON$
}

private [internal] object LexicalExit extends ScopeExit(true, false) {
  override def cleanup(ctx: Context): Unit = {
    ctx.recoveredErrors = ctx.recoveredErrors.map(x => x.markAsLexical(ctx.handlers.check))
  }
}

private [internal] final class Fail(width: CaretWidth, msgs: String*) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.failWithMessage(width, msgs: _*)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Fail(${msgs.mkString(", ")})"
    // $COVERAGE-ON$
}

private [internal] final class Unexpected(msg: String, width: CaretWidth) extends Instr {
    private [this] val unexpected = new UnexpectDesc(msg, width)
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        ctx.unexpectedFail(None, unexpected)
    }
    // $COVERAGE-OFF$
    override def toString: String = s"Unexpected($msg)"
    // $COVERAGE-ON$
}

private [internal] final class VanillaGen[A](gen: parsley.errors.VanillaGen[A]) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        // stack will have an (A, Int) pair on it
        val (x, caretWidth) = ctx.stack.pop[(A, Int)]()
        val unex = gen.unexpected(x)
        val reason = gen.reason(x)
        val err = unex.makeError(ctx.offset, ctx.line, ctx.col, gen.adjustWidth(x, caretWidth))
        // Sorry, it's faster :(
        if (reason.isDefined) ctx.fail(err.withReason(reason.get))
        else ctx.fail(err)
    }

    // $COVERAGE-OFF$
    override def toString: String = "VanillaGen"
    // $COVERAGE-ON$
}

private [internal] final class SpecializedGen[A](gen: parsley.errors.SpecializedGen[A]) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        // stack will have an (A, Int) pair on it
        val (x, caretWidth) = ctx.stack.pop[(A, Int)]()
        ctx.failWithMessage(new RigidCaret(gen.adjustWidth(x, caretWidth)), gen.messages(x): _*)
    }

    // $COVERAGE-OFF$
    override def toString: String = "SpecializedGen"
    // $COVERAGE-ON$
}
