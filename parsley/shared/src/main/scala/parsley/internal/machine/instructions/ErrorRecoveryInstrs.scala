/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

package parsley.internal.machine.instructions

import parsley.internal.machine.Context
import parsley.internal.machine.XAssert.{ensureHandlerInstruction, ensureRegularInstruction}
import parsley.internal.machine.errors.NoError



/**
  * This instruction is used when a recoverWith is used by the initial parser succeeds without issue
  */
private [internal] class SucceedWithoutRecoveryAndJump(var label: Int, val producesResults: Boolean) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
        if(producesResults) {
            val item = ctx.stack.upop()
            ctx.stack.push(Left(item))
        }

        assert(!ctx.errorState.isLive, "Cannot succeed without recovery with live errors");
        ctx.handlers = ctx.handlers.tail
        ctx.popAndMergeErrors()

        ctx.pc = label
        
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SucceedWithoutRecoveryAndJump($label)"
    // $COVERAGE-ON$
}


/**
  * Used when a recoverWith fails and begins recovering
  */
  private [internal] class BeginRecovery(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        
        ensureHandlerInstruction(ctx)


        // If we accumulated some data on the stack before failing we need to clear it
        val handler = ctx.handlers
        val stackToDiscard = ctx.stack.usize - handler.stacksz 
        if(stackToDiscard > 0) {
            ctx.stack.drop(stackToDiscard)
        }
        // Update this handler to point to failure of recovery parser
        // TODO (Dan) need to figure out how error stacks work here
        assert(ctx.errorState.isLive, "Cannot begin recovery if there is no error")
   
        // Move error onto a recovery stack
        ctx.moveErrorToRecovery()
        handler.pc = label
        ctx.good = true
        ctx.inc()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"BeginRecovery($label)"
    // $COVERAGE-ON$
}


/**
  * We have successfully finished parsing the recovery parser
  *
  */
private [internal] class SucceedRecoveryAndJump(var label: Int, val producesResults: Boolean) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
  
        assert(!ctx.errorState.isLive, "Cannot finish recovery with live errors");

        if(producesResults) {
            val item = ctx.stack.upop()
            ctx.stack.push(Right(item))
        }


        // Move the error from recovery stack to list of actual errors
        ctx.handlers = ctx.handlers.tail

        ctx.commitRecoveredError();
        ctx.errorState = NoError
        ctx.popAndMergeErrors()
        
        ctx.pc = label
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SucceedRecoveryAndJump($label)"
    // $COVERAGE-ON$
}


/**
  * Recovery Failed
  * We need to resupply the error message from the original parser rather than the recovered one
  *
  */
  private [internal] class FailRecovery extends Instr {
    override def apply(ctx: Context): Unit = {
        ensureHandlerInstruction(ctx)

        assert(ctx.errorState.isLive, "Recovery must have thrown a live error");

        // Move the error from recovery stack to list of actual errors
        ctx.replaceRecoveryError()
        ctx.handlers = ctx.handlers.tail
        ctx.popAndMergeErrors()
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"FailRecovery"
    // $COVERAGE-ON$
}
