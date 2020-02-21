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

    def init[A](l: List[A]): List[A] = l match {
        case Nil => sys.error("Empty list.")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
    
    @annotation.tailrec
    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match { 
        case Nil => z
        case Cons(h,t) => foldLeft(t, f(z,h))(f)
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

    def length2[A](as: List[A]): Int = {
        List.foldLeft(as, 0)((acc, _) => acc + 1)
    }

    def sum3(ns: List[Int]) = {
        List.foldLeft(ns, 0)(_+_)
    }

    def product3(ns: List[Int]) = {
        List.foldLeft(ns, 1)(_*_)
    }

    def reverse[A](ns: List[A]): List[A] = {
        List.foldLeft(ns, List[A]())((acc,h) => Cons(h,acc))
        }

    def main(args: Array[String]): Unit = {
        val y = List(1,2,3,4,5)
        println(y)
        println(reverse(y))
    }
}