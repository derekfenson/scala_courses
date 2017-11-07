package myList

import math.Ordering



object myList {

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(_) => List()
    case y :: ys => y :: init(ys)
  }

  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => reverse(ys) ++ List(y)
  }

  def removeAt(n: Int, xs: List[Int]) =
    (xs take n) ++ (xs drop n+1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case y :: ys => y match {
      case List() => flatten(ys)
      case z :: zs => z :: flatten(zs :: ys)
      case z => z :: flatten(xs.tail)
    }
  }
/*
  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length/2
    if (n==0) xs
    else {
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  def merge(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (x :: xs1, y :: ys1) =>
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
  */

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (xs, Nil) => xs
          case (Nil, ys) => ys
          case (x :: xs1, y :: ys1) =>
            if (ord.lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }

      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  def squareList1(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y*y :: squareList1(ys)
  }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x*x)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil     => Nil
    case y :: _  =>
      val (first, rest) = xs span (_==y)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (x => (x.head, x.length))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(f(_) :: _)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((x, y) => y+1)
}

object Main extends App {
  import myList._
  val list1=List(2,1,7,9,-10,12)
  val list2=msort(list1)
  val list3=List('a','a','a','b','b','c', 'a','a')
  println(pack(list3))
  println(encode(list3))
  println(list3.length)
  println(lengthFun(list3))
}

