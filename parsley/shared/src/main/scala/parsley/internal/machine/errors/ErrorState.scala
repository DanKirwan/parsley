package parsley.internal.machine.errors

private [machine] sealed trait ErrorState[+A] {
    def map[B](f: A => B): ErrorState[B]
    def flatMap[B](f: A => ErrorState[B]): ErrorState[B]
    def orElse[B >: A](other: => ErrorState[B]): ErrorState[B]
    def isLive: Boolean
    def isAccumulator: Boolean
    def isEmpty: Boolean
    def get: A
  }
  
  case class LiveError[A](value: A) extends ErrorState[A] {
    def map[B](f: A => B): ErrorState[B] = LiveError(f(value))
    def flatMap[B](f: A => ErrorState[B]): ErrorState[B] = f(value)

    def orElse[B >: A](other: => ErrorState[B]): ErrorState[B] = this
    
    def isLive: Boolean = true
    def isAccumulator: Boolean = false
    def isEmpty: Boolean = false
    
    def get = value
  }
  
  case class AccumulatorError[A](value: A) extends ErrorState[A] {
    def map[B](f: A => B): ErrorState[B] = AccumulatorError(f(value))
    def flatMap[B](f: A => ErrorState[B]): ErrorState[B] = f(value)

    def orElse[B >: A](other: => ErrorState[B]): ErrorState[B] = this
    def isLive: Boolean = false
    def isAccumulator: Boolean = true
    def isEmpty: Boolean = false

    def get = value
  }
  
  case object NoError extends ErrorState[Nothing] {
    def map[B](f: Nothing => B): ErrorState[B] = NoError
    def flatMap[B](f: Nothing => ErrorState[B]): ErrorState[B] = NoError

    def orElse[B](other: => ErrorState[B]): ErrorState[B] = other
    def isLive: Boolean = false
    def isAccumulator: Boolean = false
    def isEmpty: Boolean = true

    def get: Nothing = throw new NoSuchElementException("NoError.get")
  }