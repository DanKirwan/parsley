/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.internal.machine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ArrayBuffer
import parsley.internal.machine.PositionTracker
import parsley.ap

class PositionTrackerTest extends AnyFlatSpec with Matchers {

  "PositionTracker" should "correctly track positions in a simple string" in {
    val input = "This is a test string."
    val tracker = new PositionTracker(input)

    tracker.getPos(0) shouldEqual (1, 1)
    tracker.getPos(5) shouldEqual (1, 6)
    tracker.getPos(10) shouldEqual (1, 11)
    tracker.getPos(19) shouldEqual (1, 20)
  }

  it should "handle newlines correctly" in {
    val input = "This is a\ntest string\nwith newlines."
    val tracker = new PositionTracker(input)

    tracker.getPos(0) shouldEqual (1, 1)
    tracker.getPos(10) shouldEqual (2, 1)
    tracker.getPos(11) shouldEqual (2, 2)
    tracker.getPos(20) shouldEqual (2, 11)
    tracker.getPos(30) shouldEqual (3, 9)
  }


  it should "handle tabs correctly" in {
    val input = "This is a\ttest string\twith tabs."
    val tracker = new PositionTracker(input)

    tracker.getPos(0) shouldEqual (1, 1)
    tracker.getPos(10) shouldEqual (1, 13) 
    tracker.getPos(15) shouldEqual (1, 18)
    tracker.getPos(25) shouldEqual (1, 28)
  }

  it should "handle new lines correctly" in {
    val input = "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"
    val tracker = new PositionTracker(input)

    tracker.getPos(0) shouldEqual (1, 1)
    tracker.getPos(7) shouldEqual (2, 1)
    tracker.getPos(14) shouldEqual (3, 1)
    tracker.getPos(21) shouldEqual (4, 1)
    tracker.getPos(28) shouldEqual (5, 1)
  }

  it should "handle edge cases correctly" in {
    val input = ""
    val tracker = new PositionTracker(input)

    tracker.getPos(0) shouldEqual (1, 1)
    // Out of bounds
    assertThrows[IndexOutOfBoundsException] {
      tracker.getPos(1)
    }
  }

  it should "handle continuous movement" in {
    val input = "Continuous\nmovement\nthrough\ntext."
    val tracker = new PositionTracker(input)

    tracker.getPos(0) shouldEqual (1, 1)
    tracker.getPos(10) shouldEqual (1, 11)
    tracker.getPos(25) shouldEqual (3, 6)
    tracker.getPos(20) shouldEqual (3, 1)
    tracker.getPos(30) shouldEqual (4, 3)
    tracker.getPos(25) shouldEqual (3, 6)
    tracker.getPos(15) shouldEqual (2, 5)
    tracker.getPos(5) shouldEqual (1, 6)
  }
}