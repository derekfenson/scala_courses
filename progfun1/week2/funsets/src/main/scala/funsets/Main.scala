package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(forall(singletonSet(1), (x: Int) => x==1))
}
