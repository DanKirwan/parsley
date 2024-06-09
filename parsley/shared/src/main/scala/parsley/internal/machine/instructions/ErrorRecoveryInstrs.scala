/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

package parsley.internal.machine.instructions

import parsley.internal.machine.Context
import parsley.internal.machine.XAssert.{ensureHandlerInstruction, ensureRegularInstruction}



/**
  * This instruction is used when a recoverWith is used by the initial parser succeeds without issue
  */
private [internal] class SucceedWithoutRecoveryAndJump(var label: Int, val producesResults: Boolean) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)

        assert(!ctx.isLiveError, "Cannot succeed without recovery with live errors");
        ctx.popAndMergeErrors()
        ctx.handlers = ctx.handlers.tail

        ctx.pc = label
        
    }
    // $COVERAGE-OFF$
    override def toString: String = s"SucceedWithoutRecoveryAndJump($label)"
    // $COVERAGE-ON$
}


/**
  * Used when a recoverWith fails and begins recovering
  */
  private [internal] class RecoveryPoint(var label: Int) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        
        ensureHandlerInstruction(ctx)

        if(!ctx.forceRecovery) {
          
          assert(ctx.isLiveError, "Cannot setup recovery if there is no error")
          // Move error onto a recovery stack
          ctx.pushRecoveryPoint()

          // We're setting up the recovery point here to be recovered to later
          ctx.popAndMergeErrors()
          ctx.handlers = ctx.handlers.tail
          // TODO This can be reset to 0 as it's used only as an optimization but 
          // it could be set higher if we have more information on previous deepest errors
          ctx.deepestError = 0
          ctx.fail()
        } else {
          
            ctx.forceRecovery = false
            ctx.setupRecovery()
            assert(!ctx.isLiveError, "Error should have been moved to parked recovery before beginning recovery")
            // If we accumulated some data on the stack before failing we need to clear it
            val handler = ctx.handlers
            val stackToDiscard = ctx.stack.usize - handler.stacksz 
            if(stackToDiscard > 0) {
              ctx.stack.drop(stackToDiscard)
            }

            handler.pc = label
            ctx.good = true
            ctx.inc()
          }
        }
    // $COVERAGE-OFF$
    override def toString: String = s"RecoveryPoint($label)"
    // $COVERAGE-ON$
}


/**
  * We have successfully finished parsing the recovery parser
  *
  */
private [internal] class SucceedRecoveryAndJump(var label: Int, val producesResults: Boolean) extends InstrWithLabel {
    override def apply(ctx: Context): Unit = {
        ensureRegularInstruction(ctx)
  
        assert(!ctx.isLiveError, "Cannot finish recovery with live errors");



        // Move the error from recovery stack to list of actual errors
        
        ctx.succeedRecovery();
        ctx.errorState = None
        ctx.popAndMergeErrors()
        
        ctx.handlers = ctx.handlers.tail
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

        assert(ctx.isLiveError, "Recovery must have thrown a live error");

        // Move the error from recovery stack to list of actual errors
        ctx.failRecovery()
        ctx.popAndMergeErrors()
        ctx.handlers = ctx.handlers.tail
        ctx.fail()
    }
    // $COVERAGE-OFF$
    override def toString: String = s"FailRecovery"
    // $COVERAGE-ON$
}
