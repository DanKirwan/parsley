package parsley.internal.machine

import stacks.{LazyCopyStack, CallStack,  ErrorStateStack, HandlerStack,  StateStack}
import errors.DefuncError
import instructions.Instr

import parsley.internal.machine.stacks.Stack.StackExt

private [machine] class RecoveryState(
    val handlers: HandlerStack, 
    val data: LazyCopyStack[Any], val callStack: CallStack, val states: StateStack, 
    val regs: Array[AnyRef], val instrs: Array[Instr],
    val currentError: DefuncError, val recoveredErrors: List[DefuncError],
    val parkedError: Option[DefuncError], val recoveryDepth: Int,
    val pc: Int, val offset: Int, val check: Int) {


        override def toString(): String = s"Recovery(pc=$pc, ${callStack.mkString(" -")})"
}