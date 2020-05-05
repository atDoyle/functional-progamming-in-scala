// A comment
/* Another comment */
/** A documentation comment */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  // Higher order functions, recursive functions
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = 
      if (n<=0) acc
      else go(n-1, n*acc)
    
    go(n, 1)
  }

  // Exercise 2.1, nth Fibonacci function
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (gt(as(n), as(n+1))) false
      else go(n+1)

    go(0)
  }

    // Exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
      a => b => f(a, b)

  // Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)


  // Exercise 2.5
  def compose[A,B,C](f:B => C, g: A => B): A => C = 
    a => f(g(a))

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)
    loop(0)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    println(msg.format(name, n, f(n)))
  }

  def main(args: Array[String]): Unit =
    formatResult("absolute value", -42, abs)
    formatResult("factorial", 8, factorial)
}