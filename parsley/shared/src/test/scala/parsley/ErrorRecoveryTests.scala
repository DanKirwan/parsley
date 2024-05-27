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
import parsley.unicode.satisfy
import parsley.state._
import parsley.lift._



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

    "multiple recovery" should "create a multifailure if fail after previous recoveries" in {
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

    it should "accumulate and allow error operations on the relevant messages" in {
        inside( ( recoverWith('a', 'b') <* recoverWith('x', 'y').label("test")).parse("bb") ) {
            case MultiFailure(
                TestError((1,2), VanillaError(unex1, exs1, rs1, 1)) :: 
                TestError((1,1), VanillaError(unex2, exs2, rs2, 1)) :: 
                    Nil) => 
                unex1 should contain (Raw("b"))
                exs1 should contain only (Named("test"))
                unex2 should contain (Raw("b"))
                exs2 should contain only Raw("a")
                rs1 shouldBe empty
                rs2 shouldBe empty

        }
    }

    // Displays the rule recoverWith(recoverWith(a, b), c) == recoverWith(a, b | c)
    "nested recovery" should "attempt latest first" in {
        inside(recoverWith(recoverWith('a' , 'b' *> pure('x')), 'b' *> pure('y')).parse("b")) {
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, _, _)) :: Nil) =>
                result shouldBe 'x'
                unex should contain (Raw("b"))
                exs should contain only (Raw("a")) 
        }
    }


    "error recovery" should "apply labels regardless if recovered" in {
        val rec = recoverWith('a', 'b').label("test")
        (rec.parse("a")) shouldBe a [Success[_]]

        inside(rec.parse("b")) {
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, rs, 1)) :: Nil) => 
                result shouldBe 'b'
                unex should contain (Raw("b"))
                exs should contain only Named("test")
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
                result shouldBe 'c'
                unex should contain (Raw("c"))
                exs should contain only Raw("a")
        }

    }

    it should "not attempt recovery in non fatal scenarios" in {
        val rec = recoverWith('a', 'c') | 'c'
        // Confirming this isn't a recovered error
        rec.parse("c") shouldBe Success('c') 


        // Example used to ensure no jump table optimizations
        val rec2 = recoverWith('a', digit *> pure("test"))  | digit
        rec2.parse("0") shouldBe Success('0')

    }

    it should "use recovery if parser fails fatally in retrospet" in {
        
        inside((recoverWith('a', 'c') | 'b').parse("c")) {
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, rs, 1)) :: Nil) => 
                result shouldBe 'c'
                unex should contain (Raw("c"))
                exs should contain only Raw("a")
        }
    }

    it should "recover at the deepest point first" in {

        val failFirst = atomic('a' *> recoverWith('z' *> 'c' , 'b' *> 'x' *> pure('E')));
        val failSecond = atomic('a' *> 'b' *> recoverWith('c', 'x' *> pure('L')));


        inside(failFirst.parse("abx")) {
            case Recovered(result, TestError((1,2), _) :: Nil) => result shouldBe 'E'
        }

        
        inside(failSecond.parse("abx")) {
            case Recovered(result, TestError((1,3), _) :: Nil) => result shouldBe 'L'
        }


        inside((failSecond | failFirst).parse("abx")) {
            case Recovered(result, TestError((1,3), _) :: Nil) => result shouldBe 'L'
        }

        inside((failFirst| failSecond).parse("abx")) {
            case Recovered(result, TestError((1,3), _) :: Nil) => result shouldBe 'L'
        }

    }


    it should "be invisible if reocvery also fails" in {
        inside((amend("zzz" *> dislodge(recoverWith(entrench('a'), 'b')))).parse("zzzx")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs, 1))) =>
                unex should contain (Raw("z"))
                exs should contain only (Raw("a"))
        }

        inside((amend("zzz" *> recoverWith(entrench('a'), 'b'))).parse("zzzx")) {
            case Failure(TestError((1, 4), VanillaError(unex, exs, rs, 1))) =>
                unex should contain (Raw("x"))
                exs should contain only (Raw("a"))
        }

        inside((amend("zzz" *> recoverWith('a', 'b'))).parse("zzzx")) {
            case Failure(TestError((1, 1), VanillaError(unex, exs, rs, 1))) =>
                unex should contain (Raw("z"))
                exs should contain only (Raw("a"))
        }
    }
    

    "recovered errors" should "be affected by combinators outside the recover scope" in {
        inside((amend("zzz" *> recoverWith('a', 'b'))).parse("zzzb")) {
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, rs, 1)) :: Nil) => 
                result shouldBe 'b'
                unex should contain (Raw("z"))
                exs should contain only Raw("a")

        }

        inside((amend("zzz" *> recoverWith('a', 'b')).label("test")).parse("zzzb")) {
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, rs, 1)) :: Nil) => 
                result shouldBe 'b'
                unex should contain (Raw("z"))
                exs should contain only Named("test")

        }

        inside(recoverWith('a', 'b').label("test").parse("b")) {
            case Recovered(result, TestError((1,1), VanillaError(unex, exs, rs, 1)) :: Nil) => 
                result shouldBe 'b'
                unex should contain (Raw("b"))
                exs should contain only (Named("test"))
        }
    }

    "fatal errors" should "be affected by combinators outside the recover scope" in {
        inside((amend("zzz" *> recoverWith('a', 'b'))).parse("zzzz")) {
            case Failure(TestError((1,1), VanillaError(unex, exs, rs, 1))) => 
                unex should contain (Raw("z"))
                exs should contain only Raw("a")

        }

        inside((amend("zzz" *> recoverWith('a', 'b')).label("test")).parse("zzzz")) {
            case Failure(TestError((1,1), VanillaError(unex, exs, rs, 1))) => 
                unex should contain (Raw("z")) 
                exs should contain only (Named("test"))

        }

        inside((recoverWith('a', 'b')).label("test").parse("z")) {
            case Failure(TestError((1,1), VanillaError(unex, exs, rs, 1))) => 
                unex should contain (Raw("z"))
                exs should contain only (Named("test"))
        }
    }


    "failed recovery" should "not not keep error messages from failed previous recovery attempts if an attempt is successful" in {
        
        val failFirst = atomic(recoverWith('z' *> 'c' , 'b' *> 'x' *> pure('1')));
        val failSecond = atomic('b' *> recoverWith('c', 'x' *> pure('2') *> Parsley.empty));
        val failSecondAfterRecover = atomic('b' *> recoverWith('c', 'x' *> pure('2')) *> Parsley.empty);

        inside((failSecond | failFirst).parse("bx")) {
            case Recovered(result, TestError((1,1), _) :: Nil) => result shouldBe '1'
        }

        inside((failSecondAfterRecover | failFirst).parse("bx")) {
            case Recovered(result, TestError((1,1), _) :: Nil) => result shouldBe '1'
        }
        
        inside((failFirst | failSecond).parse("bx")) {
            case Recovered(result, TestError((1,1), _) :: Nil) => result shouldBe '1'
        }

        inside((failFirst | failSecondAfterRecover).parse("bx")) {
            case Recovered(result, TestError((1,1), _) :: Nil) => result shouldBe '1'
        }
    }


    it should "follow the path of deepest recovery if no recovered parse is possible" in {
        
        val failFirst = atomic(recoverWith('z' *> 'c' , 'b' *> 'x' *> pure('1') *> Parsley.empty));
        val failFirstAfterRecover = atomic(recoverWith('z' *> 'c' , 'b' *> 'x' *> pure('1')) *> Parsley.empty);
        val failSecond = atomic('b' *> recoverWith('c', 'x' *> pure('2') *> Parsley.empty));
        val failSecondAfterRecover = atomic('b' *> recoverWith('c', 'x' *> pure('2')) *> Parsley.empty);

        // the logic here is that anything with a failSecond should be a single failure
        // as the error occurs later - it shouldn't try to recover down another path because it's 
        // known to fail
        inside((failFirst | failSecondAfterRecover).parse("bx")) {
            case MultiFailure(TestError((1, 3), _) :: TestError((1,2), _) :: Nil) => 
        }

        inside((failFirstAfterRecover | failSecond).parse("bx")) {
            case Failure(TestError((1,2), _)) => 
        }

        inside((failFirstAfterRecover | failSecondAfterRecover).parse("bx")) {
            case MultiFailure(TestError((1, 3), _) :: TestError((1,2), _) :: Nil) => 
        }
        
        inside((failFirst | failSecond).parse("bx")) {
            case Failure(TestError((1,2), _)) => 
        }

        // testing ordering doesn't matter 

        inside((failSecondAfterRecover | failFirst ).parse("bx")) {
            case MultiFailure(TestError((1, 3), _) :: TestError((1,2), _) :: Nil) => 
        }

        inside((failSecond | failFirstAfterRecover ).parse("bx")) {
            case Failure(TestError((1,2), _)) => 
        }

        inside((failSecondAfterRecover | failFirstAfterRecover ).parse("bx")) {
            case MultiFailure(TestError((1, 3), _) :: TestError((1,2), _) :: Nil) => 
        }
        
        inside((failSecond | failFirst ).parse("bx")) {
            case Failure(TestError((1,2), _)) => 
        }
        


    }



    "flatmap error recovery" should "function without state even after leaving DynCall" in {
        val iNoState = recoverWith('a', 'b')
        val k = 'x'.flatMap(_ => iNoState) | "xx"

        inside(k.parse("xb")) {case Recovered('b', TestError((1,2), _) :: Nil) =>}


        inside((k *> k).parse("xbxb")) {case Recovered('b', TestError((1,4), _) ::  TestError((1,2), _):: Nil) =>}


        
    }

    // it should "handle recursive case" in {
    //     var x: Parsley[Unit] = pure(())
    //     var death: Parsley[Unit] = 'x'.flatMap(_ => x)
    //     x = death *> 1.makeRef(_=> pure('a'))

    //     death.parse("xxx")
    // }


    "stateful error recovery" should "not effect persistent state in successful parses" in {
        val p = 5.makeRef { r1 =>
            7.makeRef { r2 =>
                recoverWith(r1.set(lift2[Int, Int, Int](_+_, r1.get, r2.get)) *> (r1.get zip r2.gets(_+1)), Parsley.empty)
            }
        }
        p.parse("") should be (Success((12, 8)))
    }


    it should "treat state as though it ran only the final parse path" in {
        
        val r1 = Ref.make[Int]
        
        val p = r1.set(5) *> (recoverWith(r1.update(_+1) *> Parsley.empty, r1.update(_+1)) | r1.update(_+1)) *> r1.get

        p.parse("") should be (Success(7))


        val pFail = r1.set(5) *> (recoverWith(r1.update(_+1) *> Parsley.empty, r1.update(_+1)) | r1.update(_+1) *> Parsley.empty) *> r1.get

        inside(pFail.parse("")) { case Recovered(7, _) => }


    }

    it should "keep state from within recovery parser" in {
        val r2 = Ref.make[String]
        val pFailString = r2.set("init") *> (recoverWith(r2.set(pure("f1")) *> Parsley.empty, r2.set(pure("recovery"))) | r2.set(pure("f2")) *> Parsley.empty) *> r2.get

        inside(pFailString.parse("")) { case Recovered("recovery", _) => }

    }

    they should "be modifiable" in {
        val p = 5.makeRef(r1 => r1.update(_+1) *> r1.get)
        p.parse("") should be (Success(6))
    }
    they should "provide localised context" in {
        val r1 = Ref.make[Int]
        val p = r1.set(5) *> (r1.updateDuring(_+1)(r1.get) zip r1.get)
        val q = r1.set(5) *> (r1.setDuring(6)(r1.get) <~> r1.get)
        p.parse("") should be (Success((6, 5)))
        q.parse("") should be (Success((6, 5)))
    }
    they should "be correctly allocated when found inside recursion" in {
        val r1 = Ref.make[Int]
        lazy val rec: Parsley[Unit] = 'a' *> r1.set(1) *> rec <|> unit
        val p = "hello :)".makeRef(r2 => rec *> r2.get)
        p.parse("a") shouldBe Success("hello :)")
    }
    they should "be correctly allocated when found inside sub-routines" in {
        val r1 = Ref.make[Int]
        val q = 'a' *> r1.set(1)
        val p = "hello :)".makeRef(r2 => q *> q *> r2.get)
        p.parse("aa") shouldBe Success("hello :)")
    }
    they should "be preserved by callee-save in flatMap" in {
        val p = "hello world".makeRef(r2 => {
            6.makeRef(r1 => {
                unit.flatMap(_ => 4.makeRef(_ => r2.set("hi"))) *>
                (r1.get <~> r2.get)
            })
        })
        p.parse("") shouldBe Success((6, "hi"))
    }
    they should "be preserved by callee-save in flatMap even when it fails" in {
        val p = "hello world".makeRef(r2 => {
            6.makeRef(r1 => {
                combinator.optional(unit.flatMap(_ => r2.set("hi") *> 4.makeRef(_ => Parsley.empty))) *>
                (r1.get zip r2.get)
            })
        })
        p.parse("") shouldBe Success((6, "hi"))
    }
    they should "be able to be rolled back if they fail softly" in {
        val p = 3.makeRef(r1 => (r1.rollback(r1.set(2) *> Parsley.empty) <|> unit) *> r1.get)
        p.parse("") shouldBe Success(3)
    }
    they should "but not roll back if they hard fail" in {
        val p = 3.makeRef(r1 => (atomic(r1.rollback('a' *> r1.set(2) *> Parsley.empty)) <|> unit) *> r1.get)
        p.parse("a") shouldBe Success(2)
    }
    they should "not rollback if successful" in {
        val p = 3.makeRef(r1 => r1.rollback(r1.set(2)) *> r1.get)
        p.parse("") shouldBe Success(2)
    }
    they should "support more than 4 registers" in {
        def loop(n: Int): Parsley[List[Char]] = {
            if (n == 0) pure(Nil)
            else item.fillRef { c => c.get <::> loop(n-1) }
        }
        val p = loop(16)
        p.parse("abcdefghijklmnop") shouldBe Success(List('a', 'b', 'c','d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'))
    }

    // "fillReg" should "appear to create a fresh register every time its invoked" in {
    //     def inc(reg: Ref[Int]): Parsley[Int] = reg.get <* reg.update(_ + 1)
    //     val p = 0.makeRef { i =>
    //         // the register j is static, however, each recursive call should save its value, making it appear dynamic
    //         lazy val p: Parsley[List[Int]] = 'a' *> inc(i).fillRef(j => (p, j.get).zipped(_ :+ _) <* 'b') <|> pure(Nil)
    //         p
    //     }
    //     p.parse("aaabbb") shouldBe Success(List(2, 1, 0))
    // }

    it should "also appear to create a fresh register even in the presence of a hard failure" in {
        lazy val p: Parsley[Char] = item.fillRef(c => item *> (atomic(p) <|> c.get))
        p.parse("abc") shouldBe Success('a')
    }
    
    
}