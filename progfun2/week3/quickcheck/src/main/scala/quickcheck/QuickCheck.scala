package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      elem <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(elem, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a1: A, a2: A) =>
    val min = if (a1 < a2) a1 else a2
    findMin(insert(a1, insert(a2, empty))) == min
  }

  property("empty1") = forAll { (a: A) =>
    deleteMin(insert(a, empty)) == empty
  }

  property("deletemin1") = forAll { (h: H) =>
    val min = if (isEmpty(h)) 0 else findMin(h)
    val h1 = if (isEmpty(h)) h else deleteMin(h)
    findMin(insert(min, h1)) == min
  }

  def sortedHelper(h: H): List[A] = {
    if (isEmpty(h)) List()
    else
      findMin(h) :: sortedHelper(deleteMin(h))
  }

  def checkSorted(ls: List[A]): Boolean = ls match {
    case List() => true
    case x :: xs => xs match {
      case List() => true
      case y :: _ =>
        if (ord.gt(x,y)) false else checkSorted(xs)
    }
  }

  property("sorted1") = forAll { (h: H) =>
    val sortedList = sortedHelper(h)
    checkSorted(sortedList)
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m3 = if (m1 < m2) m1 else m2
    val h3 = meld(h1, h2)
    findMin(h3) == m3
  }

  property("meld2") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val h3 = meld(h1, h2)
    val newh1= insert(m2, deleteMin(h1))
    val newh2 = insert(m1, deleteMin(h2))
    sortedHelper(h3) == sortedHelper(meld(newh1, newh2))
  }


}
