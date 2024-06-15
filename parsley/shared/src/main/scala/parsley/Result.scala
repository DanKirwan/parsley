/*
 * Copyright 2020 Parsley Contributors <https://github.com/j-mie6/Parsley/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package parsley

import scala.annotation.nowarn
import scala.util.{Failure => TFailure, Success => TSuccess, Try}

/** This trait represents the result of a parser.
  *
  * Either a `Success[A]` or a `Failure`.
  *
  * @tparam A the type of expected success result.
  */
sealed abstract class Result[+Err, +A] {
    /** Returns the result of applying `ferr` to this result's error if this is a `Failure` or `fa` to the result stored in the `Success` otherwise.
      *
      * @param ferr the function to apply if this is a `Failure`.
      * @param fa the function to apply if this is a `Success`.
      * @return the results of applying the function
      * @since 1.7.0
      */
    def fold[B](ferr: Err => B, fa: A => B): B = this match {
        case Success(x)   => fa(x)
        case Failure(msg) => ferr(msg)
        case MultiFailure(fatal :: errs) => ferr(fatal)
        case Recovered(x, recoveredErrors) => fa(x) 
        case MultiFailure(Nil) => throw new Exception("Cannot fold empty MultiFailure")
    }

    /** Executes the procedure `f` if this is a `Success`. Otherwise, do nothing.
      *
      * This is equivalent to:
      * {{{
      * result match {
      *   case Success(x) => f(x)
      *   case _          => ()
      * }
      * }}}
      *
      * @param f The side-effecting function to execute.
      * @since 1.7.0
      */
    def foreach[U](f: A => U): Unit = this match {
        case Success(x) => f(x): @nowarn
        case Recovered(x, _) => f(x): @nowarn
        case _          => ()
    }

    /** Returns the successful value within the result.
      *
      * This is equivalent to:
      * {{{
      * result match {
      *   case Success(x) => x
      *   case _          => throw new Exception
      * }
      * }}}
      *
      * @note the result must not be a failure.
      * @throws java.util.NoSuchElementException if the result is a failure.
      * @since 1.7.0
      */
    def get: A

    /** Returns the value from this `Success` or the result of evaluating `default` if this is a `Failure`.
      *
      * @since 1.7.0
      */
    def getOrElse[B >: A](default: =>B): B = orElse(Success(default)).get

    /** Returns this result if it is a `Success`, otherwise return the result of evaluating `alternative`.
      *
      * @since 1.7.0
      */
    def orElse[B >: A, Errʹ >: Err](alternative: =>Result[Errʹ, B]): Result[Errʹ, B] = this match {
        case Success(_) => this
        case Recovered(_, _) => this
        case _          => alternative
    }

    /** Returns `true` if this result is a `Success` and its value is equal to `elem` (as determined by `==`),
      * returns `false` otherwise.
      *
      * @param elem    the element to test.
      * @return `true` if this is a `Success` value equal to `elem`.
      * @since 1.7.0
      */
    def contains[B >: A](elem: B): Boolean = exists(_ == elem)

    /** Returns `true` if this result is a `Failure` or returns the result of the application of
      * the given predicate to the `Success` value.
      *
      * @since 1.7.0
      */
    def forall(f: A => Boolean): Boolean = this match {
        case Success(x) => f(x)
        case Recovered(x, _) => f(x)
        case _          => true
    }

    /** Returns `false` if `Failure` or returns the result of the application of
      * the given predicate to the `Success` value.
      *
      * @since 1.7.0
      */
    def exists(p: A => Boolean): Boolean = this match {
        case Success(x) => p(x)
        case Recovered(x, _) => p(x)
        case _          => false
    }

    /** Returns the result of applying `f` to this result if it is a success. Returns
      * a failure if this result is a failure. Differs from `map` as `f` returns a result
      * instead of just a value.
      *
      * @since 1.7.0
      */
    def flatMap[B, Errʹ >: Err](f: A => Result[Errʹ, B]): Result[Errʹ, B] = this match {
        case Success(x) => f(x)
        case Recovered(x, _) => f(x)
        case _          => this.asInstanceOf[Result[Err, B]]
    }

    /** Returns the nested result if this result is a success, otherwise return this failure.
      *
      * Equivalent to `flatMap(identity[Result[Errʹ, B]])`.
      *
      * @since 1.7.0
      */
    def flatten[B, Errʹ >: Err](implicit ev: A <:< Result[Errʹ, B]): Result[Errʹ, B] = flatMap(ev)

    /** Returns a `Success` containing the result of applying `f` to this result's value if
      * this is a success. Otherwise, returns a failure.
      *
      * @since 1.7.0
      */
    def map[B](f: A => B): Result[Err, B] = this match {
        case Success(x) => Success(f(x))
        case Recovered(x, errs) => Recovered(f(x), errs)
        case _          => this.asInstanceOf[Result[Err, B]]
    }

    /** Returns `Success` with the existing value of `Success` if this is a `Success`
      * and the given predicate `p` holds for the right value,
      * or `Failure(msg)` if this is a `Success` and the given predicate `p` does not hold for the right value,
      * or `Failure` with the existing value of `Failure` if this is a `Failure`.
      *
      * @since 1.7.0
      */
    def filterOrElse[Errʹ >: Err](p: A => Boolean, msg: =>Errʹ): Result[Errʹ, A] = this match {
        case Success(x) if !p(x) => Failure(msg)
        case Recovered(x, _) if !p(x) => Failure(msg)
        case _                   => this
    }

    /** Returns a `Seq` containing the `Success` value if it exists or an empty `Seq` if this is a `Failure`.
      *
      * @since 1.7.0
      */
    def toSeq: Seq[A] = this match {
        case Success(x) => Seq(x)
        case Recovered(x, _) => Seq(x)
        case _          => Seq.empty
    }

    /** Returns a `Some` containing the `Success` value if it exists or a `None` if this is a `Failure`.
      *
      * @since 1.7.0
      */
    def toOption: Option[A] = this match {
        case Success(x) => Some(x)
        case Recovered(x, _) => Some(x)
        case _          => None
    }

    /** Converts the `Result` into a `Try` where `Failure` maps to a plain `Exception`.
      *
      * @since 1.7.0
      */
    def toTry: Try[A] = this match {
        case Success(x)   => TSuccess(x)
        case Recovered(x, _)   => TSuccess(x)
        case Failure(msg) => TFailure(new Exception(s"ParseError: $msg"))
        case MultiFailure(errs) => TFailure(new Exception(s"ParseErrors: ${errs.mkString(", ")}"))
    }

    /** Converts the `Result` into a `Either` where `Failure` maps to a `Left[Err]`.
      *
      * @since 1.7.0
      */
    def toEither: Either[Err, A] = this match {
        case Success(x)   => Right(x)
        case Recovered(x, _)   => Right(x)
        case Failure(msg) => Left(msg)
        case MultiFailure(msg :: _) => Left(msg)
        case MultiFailure(Nil) => throw new Exception("Cannot map empty MultiFailure to either")
    }


    
    /** Collects all errors that were thrown in the process of this parse including recovered errors
      *
      * @since 5.?
      */
    def toErrors: Seq[Err] = this match {
      case Recovered(x, recoveredErrors) => recoveredErrors.toSeq
      case MultiFailure(errs) => errs.toSeq
      case Failure(err) => Seq(err)
      case _ => Seq.empty
    }

    

    /** Returns `true` if this is a `Success`, `false` otherwise.
      *
      * @since 1.7.0
      */
    def isSuccess: Boolean

    /** Returns `true` if this is a `Failure`, `false` otherwise.
      *
      * @since 1.7.0
      */
    def isFailure: Boolean
}

/** This class is used for when a parser succeeds, and contains its result.
  *
  * @param x the result value of the successful parse.
  * @tparam A the type of expected success result.
  */
final case class Success[A](x: A) extends Result[Nothing, A] {
    /** @inheritdoc */
    override def isSuccess: Boolean = true
    /** @inheritdoc */
    override def isFailure: Boolean = false
    /** @inheritdoc */
    override def get: A = x
}

/** This class is used for a parser failure, and contains the error message.
  *
  * @tparam Err the type of the error message generated by the failing parse.
  * @param msg the error message reported by the parser.
  */
final case class Failure[Err](msg: Err) extends Result[Err, Nothing] {
    /** @inheritdoc */
    override def isSuccess: Boolean = false
    /** @inheritdoc */
    override def isFailure: Boolean = true
    /** @inheritdoc */
    override def get: Nothing = throw new NoSuchElementException("get called on Failure") // scalastyle:ignore throw
}


final case class Recovered[A, Err](x: A, val recoveredErrors: List[Err]) extends Result[Err, A] {
  override def isSuccess: Boolean = true
  /** @inheritdoc */
  override def isFailure: Boolean = false
  /** @inheritdoc */
  override def get: A = x



}

final case class MultiFailure[A, Err](errs: List[Err]) extends Result[Err, Nothing] {
  override def isSuccess: Boolean = false
  /** @inheritdoc */
  override def isFailure: Boolean = true
  /** @inheritdoc */
  override def get: Nothing = throw new NoSuchElementException("get called on Failure") // scalastyle:ignore throw




}