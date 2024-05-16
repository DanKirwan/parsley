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

/**
  * This is used at the beginning of a new choice option 
  * It will merge the existing errors in live into the choice accumulator
  *
  */
private [internal] final object ClearLiveError extends Instr {

    override def apply(ctx: Context): Unit = {
        ctx.errorToAccumulator()
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ClearLiveError";
    // $COVERAGE-ON$

}

/**
  * This is used once all choices are completed and we want to leave the choice operator 
  * with all errors merged
  *
  */
private [internal] final object ApplyErrorAccumulator extends Instr {
    override def apply(ctx: Context): Unit = {
        ctx.applyChoiceAccumulator()
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"ApplyErrorAccumulator";
    // $COVERAGE-ON$

}


private [internal] final class RelabelHints(labels: Iterable[String]) extends Instr {
    private [this] val isHide: Boolean = labels.isEmpty
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        // TOOD 
        // if hiding - remove everythig in hints? - relies on knowing where handler takes hints? - amend?
        // if this was a hide, pop the hints if possible
        // this is desirable so that hide is _very_ aggressive with labelling:
        // whitespaces.hide should say nothing, but digits.label("integer") should give digit as a hint if one is parsed, not integer
        if (isHide) ctx.popHints()
        // EOK
        // replace the head of the hints with the singleton for our label
        else if (ctx.offset == ctx.handlers.check) ctx.replaceHint(labels)
        // COK
        // do nothing
        ctx.mergeHints()
        ctx.handlers = ctx.handlers.tail
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"RelabelHints($labels)"
    // $COVERAGE-ON$
}

private [internal] final class RelabelErrorAndFail(labels: Iterable[String]) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        // this has the effect of relabelling all hints since the start of the label combinator
        // ctx.restoreHints()
        // ctx.errs.error = ctx.useHints {
        //     // only use the label if the error message is generated at the same offset
        //     // as the check stack saved for the start of the `label` combinator.
        //     ctx.errs.error.label(labels, ctx.handlers.check)
        // }

        // TODO (Dan) replace with context calls 
        ctx.liveError = ctx.liveError.map(e => e.label(labels, ctx.handlers.check))
        ctx.handlers = ctx.handlers.tail
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"RelabelErrorAndFail($labels)"
    // $COVERAGE-ON$
}

private [internal] object HideHints extends Instr {
    override def apply(ctx: Context): Unit = {
        // TODO (Dan) when do we want hide hints vs HideErrorAndFail
        ensureRegularInstruction(ctx)
        // ctx.popHints()
        // ctx.mergeHints()
        ctx.liveError = Some(new EmptyError(ctx.offset, ctx.line, ctx.col, unexpectedWidth = 0))

        ctx.handlers = ctx.handlers.tail
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = "HideHints"
    // $COVERAGE-ON$
}

// FIXME: Gigaparsec points out the hints aren't being used here, I believe they should be!
private [internal] object HideErrorAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        // TODO (Dan) figure out difference between this and above and why we need the extra instructions 
        ensureHandlerInstruction(ctx)
        // TODO (Dan) figure out my equivalent of restore hints
        ctx.restoreHints()
        // ctx.errs.error = new EmptyError(ctx.offset, ctx.line, ctx.col, unexpectedWidth = 0)
        ctx.liveError = Some(new EmptyError(ctx.offset, ctx.line, ctx.col, unexpectedWidth = 0))
        ctx.handlers = ctx.handlers.tail
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = "HideErrorAndFail"
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
        // val err2 = ctx.errs.error
        // ctx.errs = ctx.errs.tail
        // ctx.errs.error = ctx.errs.error.merge(err2)



        // We can't know here if there will be existing errors to merge (In the case of a jump table) 
        // As if we've hit the first item of the jump table we would have to have pushed a different handler
        // TODO (Dan) look into whether this is a bad idea

        // ctx.applyChoiceAccumulator();
       


    
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "MergeErrorsAndFail"
    // $COVERAGE-ON$
}

private [internal] class ApplyReasonAndFail(reason: String) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.errs.error = ctx.errs.error.withReason(reason, ctx.handlers.check)
        ctx.liveError = ctx.liveError.map(e => e.withReason(reason, ctx.handlers.check))
        // Why does this remove a handler?
        ctx.handlers = ctx.handlers.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = s"ApplyReasonAndFail($reason)"
    // $COVERAGE-ON$
}


private [internal] class AmendAndFail private (partial: Boolean) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.restoreHints() //TODO: verify this is ok; it feels more right than the restore on the labelling
        ctx.handlers = ctx.handlers.tail
        ctx.errs.error = ctx.errs.error.amend(partial, ctx.states.offset, ctx.states.line, ctx.states.col)
        ctx.liveError = ctx.liveError.map(e => e.amend(partial, ctx.states.offset, ctx.states.line, ctx.states.col))
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

private [internal] object EntrenchAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        ctx.errs.error = ctx.errs.error.entrench
        // TODO (Dan) do we need to do anything with accumulator errors
        ctx.liveError = ctx.liveError.map(e => e.entrench)
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "EntrenchAndFail"
    // $COVERAGE-ON$
}

private [internal] class DislodgeAndFail(n: Int) extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.handlers = ctx.handlers.tail
        ctx.errs.error = ctx.errs.error.dislodge(n)
        // TODO (Dan) do we need to do anything with accumulator errors
        ctx.liveError = ctx.liveError.map(e => e.dislodge(n))
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = s"DislodgeAndFail($n)"
    // $COVERAGE-ON$
}

private [internal] object SetLexicalAndFail extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)
        ctx.errs.error = ctx.errs.error.markAsLexical(ctx.handlers.check)
        ctx.handlers = ctx.handlers.tail
        ctx.fail()
    }

    // $COVERAGE-OFF$
    override def toString: String = "SetLexicalAndFail"
    // $COVERAGE-ON$
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
