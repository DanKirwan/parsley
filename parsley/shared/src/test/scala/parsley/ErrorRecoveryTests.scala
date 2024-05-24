/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import parsley.combinator.optional
import parsley.Parsley._
import parsley.syntax.character.{charLift, stringLift}
import parsley.character.{item, digit}
import parsley.errors.combinator.{fail => pfail, unexpected, amend, partialAmend, entrench, dislodge, amendThenDislodge, /*partialAmendThenDislodge,*/ ErrorMethods}
import parsley.errors.patterns._
import parsley.errors.SpecializedGen

class ErrorRecoveryTests extends ParsleyTest {

    "basic recovery" should "record errors but return an AST" in {
        inside(recoverWith('a', 'b').parse("b")) {
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, rs, 1)) :: Nil) => 
                result shouldBe 'b'
                unex should contain (Raw("b"))
                exs should contain only Raw("a")

        }
    }

    it should "not affect normal parses" in {

        recoverWith('a', 'b').parse("a") shouldBe Success('a')
    }

    it should "handle cases where input is ignored" in {
        val rec = recoverWith('a', 'b')

        inside( (rec *> rec).parse("ba") ) {
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, rs, 1)) :: Nil) => 
                result shouldBe 'a'
                unex should contain (Raw("b"))
                exs should contain only Raw("a")

        }

        inside( (rec <* rec).parse("ba") ) {
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, rs, 1)) :: Nil) => 
                result shouldBe 'b'
                unex should contain (Raw("b"))
                exs should contain only Raw("a")

        }

    }

    it should "fail with original error if recovery fails" in {
        inside(recoverWith('a', 'b').parse("x")) {
            case Failure(TestError((1,1), VanillaError(unex, exs, rs, 1))) => 
                exs should contain only (Raw("a"))
                unex should contain (Raw("x")) 
        }
    }

    it should "create a multifailure if fail after previous recoveries" in {
        val rec = recoverWith('a', 'b')


        inside((rec *> parsley.Parsley.empty).parse("b")) {
            case MultiFailure(TestError((1, 2), e1) :: TestError((1,1), VanillaError(unex, exs, _, 1)) :: Nil) => 
                unex should contain (Raw("b"))
                exs should contain only (Raw("a"))
        }
        inside( ( recoverWith('a', 'b') <* recoverWith('x', 'y')).parse("bb") ) {
            case MultiFailure(
                TestError((1,2), VanillaError(unex1, exs1, rs1, 1)) :: 
                TestError((1,1), VanillaError(unex2, exs2, rs2, 1)) :: 
                    Nil) => 
                unex1 should contain (Raw("b"))
                exs1 should contain only Raw("x")
                unex2 should contain (Raw("b"))
                exs2 should contain only Raw("a")
                rs1 shouldBe empty
                rs2 shouldBe empty

        }
    }


    it should "apply labels only if not recovered" in {
        val rec = recoverWith('a', 'b').label("test")
        (rec.parse("a")) shouldBe a [Success[_]]

        inside(rec.parse("b")) {
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, rs, 1)) :: Nil) => 
                result shouldBe 'b'
                unex should contain (Raw("b"))
                exs should contain only Raw("a")
        }

        inside(rec.parse("x")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs, 1))) =>
                unex should contain (Raw("x"))
                exs should contain only (Named("test"))
        }
    }

    it should "ignore everything from recovery error if both fail" in {
        val rec = recoverWith('a', 'b' ? "recovery")

        inside(rec.parse("x")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs, 1))) =>
                unex should contain (Raw("x"))
                exs should contain only (Raw("a"))
           
        }

        inside(recoverWith('a' ? "main", 'b' ? "recovery").parse("x")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs, 1))) =>
                unex should contain (Raw("x"))
                exs should contain only (Named("main"))
        }
    }


    it should "never produce errors in recovery mode" in {
        val rec = recoverWith('a', recoverWith('b', 'c'))
        inside(rec.parse("x")){
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs, 1))) =>
                unex should contain (Raw("x"))
                exs should contain only (Raw("a"))
        }

        inside(rec.parse("b")){
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, rs, 1)) :: Nil) => 
                result shouldBe 'b'
                unex should contain (Raw("b"))
                exs should contain only Raw("a")
        }
        inside(rec.parse("c")){
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, rs, 1)) :: Nil) => 
                result shouldBe 'b'
                unex should contain (Raw("b"))
                exs should contain only Raw("a")
        }

    }

}
