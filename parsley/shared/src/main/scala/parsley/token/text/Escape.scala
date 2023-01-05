/* SPDX-FileCopyrightText: © 2022 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley.token.text

import parsley.Parsley, Parsley.{attempt, empty, pure}
import parsley.character.{bit, char, digit, hexDigit, octDigit, strings}
import parsley.combinator.ensure
import parsley.errors.combinator.ErrorMethods
import parsley.implicits.zipped.Zipped3
import parsley.token.descriptions.text.{EscapeDesc, NumberOfDigits, NumericEscape}
import parsley.token.errors.ErrorConfig
import parsley.token.numeric

private [token] class Escape(desc: EscapeDesc, err: ErrorConfig, generic: numeric.Generic) {
    // NOTE: `strings`, while nice, is not perfect as it doesn't leverage a trie-based folding
    //       on the possibilities. We'll want trie-based folding here, or at least a specialised
    //       instruction that has the trie lookup logic baked in.
    // We do need to backtrack out of this if things go wrong, it's possible another escape sequence might share a lead
    private val escMapped = {
        desc.escMap.view.map {
            case (e, c) => e -> pure(c)
        }.toList match {
            case Nil => empty
            case x::xs => attempt(strings(x, xs: _*))
        }
    }

    private def boundedChar(p: Parsley[BigInt], maxValue: Int, prefix: Option[Char], radix: Int) = ErrorConfig.label(err.labelEscapeNumeric(radix)) {
        val prefixString = prefix.fold("")(c => s"$c")
        val numericTail = p.collectMsg { n =>
            val esc = err.renderEscapeNumericSequence(desc.escBegin, prefixString, n, radix)
            if (n > maxValue) {
                val maxEscape = err.renderEscapeNumericSequence(desc.escBegin, prefixString, BigInt(maxValue), radix)
                err.messageEscapeCharNumericSequenceTooBig(esc, maxEscape)
            }
            else err.messageEscapeCharNumericSequenceIllegal(esc)} {
            case n if n <= maxValue && Character.isValidCodePoint(n.toInt) => n.toInt
        }
        prefix match {
            case None => numericTail
            case Some(c) => char(c) *>
                            ErrorConfig.explain(err.explainEscapeNumericPostPrefix(c, radix))(ErrorConfig.label(err.labelEscapeNumericEnd(radix))(numericTail))
        }
    }

    // this is a really neat trick :)
    private lazy val atMostReg = parsley.registers.Reg.make[Int]
    private def atMost(n: Int, radix: Int, digit: Parsley[Char]): Parsley[BigInt] = {
        atMostReg.put(n) *> ensure(atMostReg.gets(_ > 0),
                                   digit <* atMostReg.modify(_ - 1)).foldLeft1[BigInt](0)((n, d) => n * radix + d.asDigit)
    }

    private def exactly(n: Int, full: Int, radix: Int, digit: Parsley[Char], reqDigits: Seq[Int]): Parsley[BigInt] = {
        atMost(n, radix, digit) <* atMostReg.get.guardAgainst {
            case x if x > 0 => err.messageEscapeCharRequiresExactDigits(radix, full - x, reqDigits)
        }
    }

    private lazy val digitsParsed = parsley.registers.Reg.make[Int]
    private def oneOfExactly(n: Int, ns: List[Int], radix: Int, digit: Parsley[Char]): Parsley[BigInt] = {
        val reqDigits@(m :: ms) = (n :: ns).sorted // make this a precondition of the description?
        def go(digits: Int, m: Int, ns: List[Int]): Parsley[BigInt] = ns match {
            case Nil => exactly(digits, m, radix, digit, reqDigits) <* digitsParsed.put(digits)
            case n :: ns  =>
                val theseDigits = exactly(digits, m, radix, digit, reqDigits)
                val restDigits = (
                        (attempt(go(n-m, n, ns).map(Some(_)) <* digitsParsed.modify(_ + digits)))
                    <|> (digitsParsed.put(digits) #> None)
                )
                (theseDigits, restDigits, digitsParsed.get).zipped[BigInt] {
                    case (x, None, _) => x
                    case (x, Some(y), exp) => (x * BigInt(radix).pow(exp - digits) + y) // digits is removed here, because it's been added before the get
                }
        }
        go(m, m, ms)
    }

    private def fromDesc(radix: Int, desc: NumericEscape, integer: =>Parsley[BigInt], digit: Parsley[Char]): Parsley[Int] = desc match {
        case NumericEscape.Illegal => empty
        case NumericEscape.Supported(prefix, numberOfDigits, maxValue) => numberOfDigits match {
            case NumberOfDigits.Unbounded         => boundedChar(integer, maxValue, prefix, radix)
            case NumberOfDigits.AtMost(n)         => boundedChar(atMost(n, radix, digit), maxValue, prefix, radix)
            case NumberOfDigits.Exactly(n, ns@_*) => boundedChar(oneOfExactly(n, ns.toList, radix, digit), maxValue, prefix, radix)
        }
    }

    private val decimalEscape = fromDesc(radix = 10, desc.decimalEscape, generic.zeroAllowedDecimal(None), digit)
    private val hexadecimalEscape = fromDesc(radix = 16, desc.hexadecimalEscape, generic.zeroAllowedHexadecimal(None), hexDigit)
    private val octalEscape = fromDesc(radix = 8, desc.octalEscape, generic.zeroAllowedOctal(None), octDigit)
    private val binaryEscape = fromDesc(radix = 2, desc.binaryEscape, generic.zeroAllowedBinary(None), bit)
    private val numericEscape = decimalEscape <|> hexadecimalEscape <|> octalEscape <|> binaryEscape
    val escapeCode = ErrorConfig.explain(err.explainEscapeInvalid)(ErrorConfig.label(err.labelEscapeEnd)(escMapped <|> numericEscape))
    val escapeBegin = ErrorConfig.label(err.labelEscapeSequence)(char(desc.escBegin))
    val escapeChar = escapeBegin *> escapeCode
}