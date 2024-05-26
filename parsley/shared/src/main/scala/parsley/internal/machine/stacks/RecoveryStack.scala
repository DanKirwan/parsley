package parsley.internal.machine.stacks
import parsley.internal.machine.RecoveryState

private [machine] final class RecoveryStack(var recoveryPoints: List[RecoveryState], val tail: RecoveryStack)
private [machine] object RecoveryStack extends Stack[RecoveryStack] {
    implicit val inst: Stack[RecoveryStack] = this
    type ElemTy = List[RecoveryState]
    // $COVERAGE-OFF$
    override protected def show(x: ElemTy): String = x.toString
    override protected def head(xs: RecoveryStack): ElemTy = xs.recoveryPoints
    override protected def tail(xs: RecoveryStack): RecoveryStack = xs.tail
    // $COVERAGE-ON$
}