import parsley.unicode
import parsley.token.descriptions.numeric.BreakCharDesc
import parsley.token.numeric.Generic
import parsley.token.numeric.UnsignedReal
import parsley.token.numeric.SignedReal
import parsley.token.numeric.LexemeReal
import parsley.token.descriptions.numeric.ExponentDesc.NoExponents
import parsley.token.descriptions.numeric.NumericDesc
import parsley.token.predicate.NotRequired
import parsley.token.predicate.Unicode
import parsley.token.Lexeme
import parsley.token.names.ConcreteNames
import parsley.token.names.LexemeNames
import parsley.token.predicate.Basic
import parsley.internal.machine.instructions.token.Basic
import parsley.token.names.Names
import parsley.token.descriptions.SymbolDesc
import parsley.token.errors.ErrorConfig
import parsley.token.descriptions.NameDesc
import parsley.position
import parsley.token.descriptions.LexicalDesc
import parsley.token.Lexer

import parsley.combinator
import parsley.Parsley, Parsley.*
import parsley.errors.combinator.*
import parsley.character.*
import parsley.syntax.character.{charLift, stringLift}

import parsley.character.spaces


import parsley.token.Lexer 



// combinator.sepEndBy("aa", 'b').parse("ab")

val p = ((atomic(char('a') ~> parsley.Parsley.empty) | char('b')).impure | unit) ~> char('c')
val y = (atomic(char('a') ~> parsley.Parsley.empty) | (char('b') | unit)) ~> char('c')

val q = (atomic(char('a') ~> parsley.Parsley.empty) | (char('b') | unit).impure) ~> char('c')
val x = ((atomic(char('a') ~> parsley.Parsley.empty).impure | char('b')) | unit) ~> char('c')


p.parse("a")
y.parse("a")
q.parse("a")
x.parse("a")

// Functioning
val func1 = (atomic(char('a') ~> char('z') ~> unexpected("test")) | (char('b').impure | unit)) ~> char('c')
val func2 = (atomic(char('a') ~> unexpected("test")) | (char('b') | unit).impure) ~> char('c')
val func3 = (atomic(char('a') ~> unexpected("test")).impure | (char('b') | unit)) ~> char('c')
val func4 = ((atomic(char('a') ~> unexpected("test")).impure | char('b')) | unit) ~> char('c')


func1.parse("ac")
func2.parse("a")
func3.parse("a")
func4.parse("a")





val allImpure = (((atomic(char('a') ~> parsley.Parsley.empty).impure | char('b')) | unit) ~> char('c')).impure

allImpure.parse("a")


val noneImpure = ((atomic(char('a') ~> parsley.Parsley.empty)) | char('b') | unit) ~> char('c')

noneImpure.parse("a")



val jump = (char('a') | char('b') | char('x')  | char('y') )

jump.parse("z")


// // val test = ((atomic(char('a').impure ~> char('x').impure ~> char('z').impure).impure).impure | unit).impure ~> char('c')
 

// // test.parse("ax")
// // test.parse("v")



// val labels = (amend(atomic(char('a') ~> char('b')))).label("test");


// labels.parse("b");


// // (char('a') | unexpected("bee") ? "something less cute").parse("b")




// // Example showing amend needs a stack
// val qarser = combinator.optional(char('b').label("b")) ~> amend(char('a') ~> digit).label("foo")


// qarser.parse("aa")



// (combinator.optional(char('a')) *> char('b')).label("hi").parse("e")

// // (char('a') <|> (Parsley.empty ? "something, at least")).parse("b")


// lookAhead("ab").parse("ac") 

// // Good example of making sure lookahead finishes in the right state - problem here is when it fails

// ('a' <|> lookAhead(combinator.optional(digit)) *> 'c' <|> 'b').parse("d")

// ('a' <|> lookAhead(combinator.optional(digit) *> 'c') <|> 'b').parse("d")





('b' ~> 'a' <|> amend(unexpected("bee")).explain("testing") ? "something less cute").parse("b")


// ('a' <|> notFollowedBy(digit) *> 'c' <|> 'b').parse("d")

def errorMaker(n: Int, msg: String) = atomic(combinator.exactly(n, 'a') *> ('b' <|> fail(msg)))
val pError =   errorMaker(2, "small")  | amend(errorMaker(3, "big")) 

pError.parse("a" * 4)



// val aDigit = amend('a' ~> digit)
// val farser = combinator.optional('b'.label("b")) ~> aDigit.label("foo")

// farser.parse("aa")



// (new Lexer(LexicalDesc.plain).parse("\"\\u11\"")

(pure("") *> string("abc") *> position.pos).parse("abc")


("ab" | "ac").parse("ac")

val lexer = new Lexer(LexicalDesc.plain)


lexer.nonlexeme.unsigned.decimal.parse("10")

lexer.nonlexeme.character.ascii.parse("\\u43")



val choice = atomic(string("abc")) <|> string("b") <|> string("abd") <|> string("cbe")

choice.parse("abe")


val consumeUntilSemicolon: Parsley[String] = combinator.manyTill(satisfy(_ != ';'), char(';')).map(_.mkString)

val rec1 = recoverWith('a', consumeUntilSemicolon)
val rec2 = recoverWith('a', 'b')

(rec2 *> rec2).parse("ba")
