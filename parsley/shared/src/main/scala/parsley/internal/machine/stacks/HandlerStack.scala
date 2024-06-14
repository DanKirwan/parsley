/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks


import parsley.internal.machine.errors.DefuncError
import parsley.internal.machine.instructions.Instr
import parsley.internal.machine.errors.ErrorState

private [machine] final class HandlerStack(
    val calls: CallStack,
    val instrs: Array[Instr],
    val error: DefuncError,
    val isEmptyError: Boolean,
    val recoveredErrors: List[DefuncError],
    val pc: Int,
    val stacksz: Int,
    val check: Int,
    val tail: HandlerStack)
private [machine] object HandlerStack extends Stack[HandlerStack] {
    implicit val inst: Stack[HandlerStack] = this
    type ElemTy = (Int, Int)
    // $COVERAGE-OFF$
    // TODO: needs to change
    override protected def show(x: ElemTy): String = {
        val (pc, stacksz) = x
        s"Handler:$pc(-${stacksz + 1})"
    }
    override protected def head(xs: HandlerStack): ElemTy = (xs.pc, xs.stacksz)
    override protected def tail(xs: HandlerStack): HandlerStack = xs.tail
    // $COVERAGE-ON$
}
