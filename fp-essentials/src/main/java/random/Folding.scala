package random

import scala.annotation.tailrec

object Folding extends App {


  //Not Stack-Safe
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f)) //non-tail recursive
    }
  }

  val frResult = foldRight(1 :: 2 :: 3 :: Nil, 0)(_ + _)
  println(frResult)


  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x :: xs => foldLeft(xs, f(z, x))(f) //tail recursive
    }
  }

  val flResult = foldLeft(1 :: 2 :: 3 :: Nil, 0)(_ + _)
  println(flResult)


  def map[A, B](as: List[A])(f: A => B): List[B] = {

    //    foldLeft(as,Nil:List[B])((xs,x) => f(x) :: xs)
    // or replace this with foldLeft/foldRight
    as match {
      case Nil => Nil
      case x :: xs => f(x) :: map(xs)(f) //non-tail recursive
    }
  }

  val mResult = map(1 :: 2 :: 3 :: Nil)(_ + 1)
  println(mResult)


  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("tail of empty list")
    case _ :: xs => xs
  }

  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil => sys.error("setHead on empty list")
    case _ :: xs => h :: xs
  }

  def drop[A](as: List[A], n: Int): List[A] =
    if (n <= 0) as
    else as match {
      case Nil => Nil
      case _ :: xs => drop(xs, n - 1)
    }

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case x :: xs if f(x) => dropWhile(xs, f)
    case _ => as
  }

  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case x :: xs if f(x) => dropWhile2(xs)(f)
    case _ => as
  }

  dropWhile(List(1, 2, 3), (x: Int) => x < 2)
  dropWhile2(List(1, 2, 3))(x => x < 2) // type inference works better with currying
  dropWhile2(List(1, 2, 3))(_ < 2) // type inference works better with currying


  def append[A](as: List[A], bs: List[A]): List[A] = as match {
    case Nil => bs
    case x :: xs => x :: append(xs, bs)
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("init of empty list")
    case _ :: Nil => Nil
    case x :: xs => x :: init(xs)
  }

}
