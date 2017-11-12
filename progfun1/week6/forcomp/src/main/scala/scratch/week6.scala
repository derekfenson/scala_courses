package scratch

import io.Source

object week6 {

  def combos(M: Int, N: Int): IndexedSeq[(Int, Int)] =
    (1 to M) flatMap (x => (1 to N) map (y => (x,y)))

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map{case (x,y) => x*y}.sum

  def isPrime(n: Int): Boolean =
    (2 until n) forall (d => n % d != 0)

  def comboSearch(n: Int): IndexedSeq[(Int, Int)] =
    (1 until n) flatMap
      (i => (1 until i)
        map (j => (i,j))) filter{case (x,y) => isPrime(x+y)}

  def comboFor(n: Int): IndexedSeq[(Int, Int)] =
    for {
      i <- 1 until n
      j <- 1 until i
      if isPrime(i + j)
    } yield (i, j)

  def scalarProductFor(xs: Vector[Double], ys: Vector[Double]): Double =
    (for ((x, y) <- xs zip ys ) yield x*y).sum

}

object nqueens {
  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =
      if (k==0) Set(List())
      else
        for {
          queens <- placeQueens(k-1)
          col <- 0 until n
          if isSafe(queens, col)
        } yield col :: queens
    placeQueens(n)
  }

  def isSafe(queens: List[Int], col: Int): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall  {
      case(r, c) => col != c && math.abs(col - c) != math.abs(row - r)
    }
  }

  def show(queens: List[Int]): Unit = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    println("\n" + (lines mkString "\n"))
  }
}

object polynomials {
  class Poly(terms0: Map[Int, Double]) {
    val terms=terms0 withDefaultValue 0.0

    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    def + (other: Poly): Poly =
      new Poly((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms updated (exp, (terms(exp) + coeff))
    }


    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted) yield coeff+"x^"+exp) mkString " + "

  }


}

object x {

  val in = Source.fromFile("/Users/derek/coursera/scala/progfun1/week6/forcomp/src/main/resources/forcomp/linuxwords.txt")

  val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))

  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI",
                '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")


  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit


  def wordCode(word: String): String =
    word.toUpperCase map charCode

  val wordsForNum: Map[String, Seq[String]] =
    words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet
}

object main extends App {
  import nqueens._
  import x._
  queens(4) map show
  println(words)
  println(charCode)
  println(wordCode("Java"))
  println(encode("43226"))

}
