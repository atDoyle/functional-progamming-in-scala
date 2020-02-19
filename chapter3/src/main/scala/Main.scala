package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    
    def tail[A](a: List[A]): List[A] = a match { 
        case Nil => sys.error("tail of empty list")
        case Cons(_, t) => t
    }

    def setHead[A](a: List[A], h: A): List[A] = a match {
        case Nil => List(h)
        case Cons(_, t) => Cons(h, t)
    }

    def drop[A](l: List[A], n: Int): List[A] =
        if (n <= 0) l
        else l match {
            case Nil => Nil
            case Cons(_, t) => drop(t, n-1)
        }
    
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }
}


// Exercise 3.1
object Exercises {
    def Exercise31 = {
        List(1,2,3,4,5) match {
            case Cons(x, Cons(2, Cons(4, _))) => x
            case Nil => 42
            case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
            case Cons(h, t) => h + List.sum(t)
            case _ => 101
        }
    }

    def main(args: Array[String]): Unit = {
        val x = List(1,2,3,4,5)
        println(x)
        println(List.dropWhile(x, (f: Int) => f%3!=0))
    }
}