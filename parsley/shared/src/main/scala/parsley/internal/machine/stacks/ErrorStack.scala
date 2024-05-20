/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks

import parsley.internal.machine.errors.DefuncError
import parsley.internal.machine.errors.ErrorState

private [machine] final class ErrorStack(var error: DefuncError, val tail: ErrorStack)
private [machine] object ErrorStack extends Stack[ErrorStack] {
    implicit val inst: Stack[ErrorStack] = this
    type ElemTy = DefuncError
    // $COVERAGE-OFF$
    override protected def show(x: ElemTy): String = x.toString
    override protected def head(xs: ErrorStack): ElemTy = xs.error
    override protected def tail(xs: ErrorStack): ErrorStack = xs.tail
    // $COVERAGE-ON$
}


private [machine] class ErrorStateStack(var errorState: ErrorState[DefuncError], var tail: ErrorStateStack) 

private [machine] object ErrorStateStack extends Stack[ErrorStateStack] {

    implicit val inst: Stack[ErrorStateStack] = this
    type ElemTy =  ErrorState[DefuncError]
    // $COVERAGE-OFF$
    override protected def show(x: ElemTy): String = x.toString
    override protected def head(xs: ErrorStateStack): ElemTy = xs.errorState
    override protected def tail(xs: ErrorStateStack): ErrorStateStack = xs.tail
    // $COVERAGE-ON$
}