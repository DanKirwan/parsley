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
private [machine] final class LazyCopyStack[A]() {
    private var array: ArrayStack[A] = new ArrayStack()
    private var stack: InternalCopyStack[A] = new InternalCopyStack[A](null, 0, null)

    private def catchEmpty[X]()(handler: => X): X = {
        while(array.isEmpty) this.popStack()
        handler
    }

    def push(x: A): Unit = {
        array.push(x)
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
        var remaining = x
        while(remaining > array.size && remaining != 0) {
            remaining -= array.size
            popStack()
        } 

        array.drop(remaining)
    }

    private def popStack(): Unit = {
        assert(array.isEmpty, "Cannot pop stack with existing elements")
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
        if(!array.isEmpty) {
            stack = new InternalCopyStack[A](array, stack.size + array.size, stack)
            array = new ArrayStack()
        }


        assert(array.isEmpty, "cannot get lazy copy without empty head array")
        val copy = new LazyCopyStack[A]()
        copy.stack = this.stack
        copy
    }
}
