package example

object Hello extends App {
  println("Hello")
}

object AnotherModule extends App {
  val x = new StringBuilder("Hello")
  val y = x.append(", World")
  println(x)
}