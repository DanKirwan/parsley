/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks

private  final class InternalCopyStack[A] (
    var saved: ArrayStack[A],
    val size: Int,
    val tail: InternalCopyStack[A]) 


private object InternalCopyStack {
    @inline def empty[T >: Null]: T = null
}




// Designed to replace the operational stack
// Since elements are of type Any, this serves as a optimised implementation
// Its success may result in the deprecation of the Stack class in favour of a generic version of this!
private [machine] final class LazyCopyStack[A](chunkSize: Int = LazyCopyStack.DefaultChunkSize) {
    private var array: ArrayStack[A] = new ArrayStack()
    private var stack: InternalCopyStack[A] = new InternalCopyStack[A](null, 0, null)
    val chunkCheck = chunkSize - 1

    private def catchEmpty[X]()(handler: => X): X = {
        while(array.isEmpty) this.popStack()
        handler
    }

    def push(x: A): Unit = {
        array.push(x)
        if(array.usize == chunkCheck) {
            stack = new InternalCopyStack[A](array, stack.size + chunkSize, stack)
            array = new ArrayStack()
        }
    }


    def upush(x: A): Unit = array.upush(x)

    def exchange(x: A): Unit = catchEmpty() {array.exchange(x)}

    def peekAndExchange(x: A): Any = catchEmpty() {array.peekAndExchange(x)}

    def pop_(): Unit = catchEmpty() {array.pop_()}

    def upop(): Any = catchEmpty() {array.upop()}

    def pop[B <: A](): B = upop().asInstanceOf[B]


    def upeek: Any = catchEmpty() {array.upeek}
    def peek[B <: A]: B = catchEmpty() {array.peek[B]}

    def drop(x: Int): Unit = {

        if(x > array.size) {
            val parentX = x - array.size
            val stacksToPop = parentX / chunkSize
            for(x <- (0 to stacksToPop)) {
                popStack()
            }

            val remaining = parentX % chunkSize
            array.drop(remaining)

        } else {
            array.drop(x)
        }
    }

    private def popStack(): Unit = {
        // remove the top stack, make and set a copy of the data of the next one
        array = stack.saved
        stack.saved = stack.saved.clone()
        this.stack = this.stack.tail
    }

    // $COVERAGE-OFF$
    def usize: Int = stack.size + array.usize
    def size: Int = stack.size + array.size
    def isEmpty: Boolean = size == 0
    def mkString(sep: String): String = array.mkString(sep)
    // $COVERAGE-ON$

    override def clone: LazyCopyStack[A] = {
        val copy = new LazyCopyStack[A](chunkSize)
        copy.stack = this.stack
        copy.array = this.array.clone()
        copy
    }
}
private [machine] object LazyCopyStack {
    final val DefaultChunkSize = 8
}
