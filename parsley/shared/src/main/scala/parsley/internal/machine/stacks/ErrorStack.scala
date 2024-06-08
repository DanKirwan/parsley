/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine.stacks

import parsley.internal.machine.errors.DefuncError
import parsley.internal.machine.errors.ErrorState
import parsley.internal.machine.errors.NoError
import scala.collection.mutable.ListBuffer

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


private [machine] class ErrorStateStack {
  private val stack: ListBuffer[Either[Int, (ErrorState[DefuncError], List[DefuncError])]] = ListBuffer()
  private var emptyErrorStateCount: Int = 0
  private var size: Int = 0

  def push(newState: (ErrorState[DefuncError], List[DefuncError])): Unit = {
    size += 1
    if (newState._1.isEmpty && newState._2.isEmpty) {
      emptyErrorStateCount += 1
    } else {
      if(emptyErrorStateCount > 0) {
        println(emptyErrorStateCount)
        stack += Left(emptyErrorStateCount)
        emptyErrorStateCount = 0
      }    
      stack += Right(newState)
    }
  }

  def pop(): (ErrorState[DefuncError], List[DefuncError]) = {
    size -= 1

    if (emptyErrorStateCount > 0) {
      println("decrementingEmptyState")
      // Decrement the counter if there are empty error states
      emptyErrorStateCount -= 1
      (NoError, List.empty[DefuncError])
    } else if (stack.nonEmpty) {

      stack.remove(stack.size - 1) match {
        case Left(value) => {
          emptyErrorStateCount += value
          emptyErrorStateCount -= 1
          (NoError, List.empty[DefuncError])
        }

        case Right(value) => value
      }
    
    } else {
      throw new Error("Cannot pop from empty stack")
    }
  }

  
  def isEmpty = size == 0
  }
