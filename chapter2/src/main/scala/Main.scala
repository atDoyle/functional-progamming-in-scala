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

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    println(msg.format(name, n, f(n)))
  }

  def main(args: Array[String]): Unit =
    formatResult("absolute value", -42, abs)
    formatResult("factorial", 8, factorial)
}

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)
    loop(0)
  }
