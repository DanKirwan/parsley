/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import scala.Predef.{String => ScalaString, _}

import parsley.Parsley, Parsley.empty
import parsley.character.{char, charUtf16}
import parsley.errors.combinator.ErrorMethods
import parsley.token.descriptions.text.TextDesc
import parsley.token.errors.ErrorConfig

private [token] final class ConcreteCharacter(desc: TextDesc, escapes: Escape, err: ErrorConfig) extends Character {
    private val quote = char(desc.characterLiteralEnd)
    private lazy val graphic = Character.letter(desc.characterLiteralEnd, desc.escapeSequences.escBegin, allowsAllSpace = false, desc.graphicCharacter)

    private def charLetter(graphicLetter: Parsley[Int]) = {
        val _checkNoBadChar = err.preemptCharBadCharsUsedInLiteral.foldLeft(empty) {
            case (w, (c, reason)) => w <|> charUtf16(c).unexpected(reason)
        }
        escapes.escapeChar <|> ErrorConfig.label(err.labelGraphicCharacter)(graphicLetter) <|> _checkNoBadChar
    }
    private def charLiteral[A](letter: Parsley[A], end: Option[ScalaString]) = quote *> letter <* ErrorConfig.label(end)(quote)

    override lazy val fullUtf16: Parsley[Int] = ErrorConfig.label(err.labelCharUtf16)(charLiteral(charLetter(graphic.toUnicode), err.labelCharUtf16End))
    // this is a bit inefficient, converting to int and then back to char, but it makes it consistent, and can be optimised anyway
    private lazy val uncheckedBmpLetter = charLetter(graphic.toBmp.map(_.toInt))

    private def constrainedBmp(illegal: Int => Boolean, label: Option[ScalaString], endLabel: Option[ScalaString],
                               unex: Option[Int => ScalaString], reason: Option[Int => ScalaString]) = {
        ErrorConfig.label(label) {
            charLiteral(ErrorConfig.unexpectedWhenWithReason(illegal, unex, reason)(uncheckedBmpLetter).map(_.toChar), endLabel)
        }
    }

    override lazy val basicMultilingualPlane: Parsley[Char] =
        constrainedBmp(!Character.isBmpCodePoint(_), err.labelCharBasicMultilingualPlane, err.labelCharBasicMultilingualPlaneEnd,
                       err.unexpectedCharNonBasicMultilingualPlane, err.explainCharNonBasicMultilingualPlane)
    override lazy val ascii: Parsley[Char] =
        constrainedBmp(_ > Character.MaxAscii, err.labelCharAscii, err.labelCharAsciiEnd, err.unexpectedCharNonAscii, err.explainCharNonAscii)
    override lazy val latin1: Parsley[Char] =
        constrainedBmp(_ > Character.MaxLatin1, err.labelCharLatin1, err.labelCharLatin1End, err.unexpectedCharNonLatin1, err.explainCharNonLatin1)
}
