package parsley.internal.machine

import stacks.{ArrayStack, CallStack,  ErrorStateStack, HandlerStack,  StateStack}
import errors.DefuncError

private [machine] class RecoveryState(
    val errorStack: ErrorStateStack, val handlers: HandlerStack, 
    val data: ArrayStack[Any], val callStack: CallStack, val states: StateStack, 
    val regs: Array[AnyRef],
    val currentError: DefuncError, val recoveredErrors: List[DefuncError],
    val parkedError: Option[DefuncError], val recoveryDepth: Int,
    val pc: Int,val offset: Int, val line: Int, val col: Int) {


        override def toString(): String = s"Recovery(($line, $col), pc=$pc)"
}