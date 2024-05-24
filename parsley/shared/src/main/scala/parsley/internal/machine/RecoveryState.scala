package parsley.internal.machine

import stacks.{ArrayStack, CallStack,  ErrorStateStack, HandlerStack,  StateStack}
import errors.DefuncError

private [machine] class RecoveryState(
    val errorStack: ErrorStateStack, val handlers: HandlerStack, 
    val data: ArrayStack[Any], val callStack: CallStack, val states: StateStack, 
    val currentError: DefuncError,
    val pc: Int,val offset: Int, val line: Int, val col: Int) {

}