package Lab1

object Lab1 {

  val merge: (List[Int], List[Int]) => List[Int] = {
    case (Nil, m2) => m2
    case (m1, Nil) => m1
    case (a :: m1, b :: m2) =>
      if (a <= b)
        a :: merge(m1, b :: m2)
      else
        b :: merge(a :: m1, m2)
  }


  def main(args: Array[String]): Unit = {
    val m1: List[Int] = List(10, 20, 50, 70)
    val m2: List[Int] = List(30, 40, 60, 80)
    println(merge(m1, m2))

    val m3: List[Int] = List(1, 2, 5, 7)
    val m4: List[Int] = List(1, 4, 5, 9)
    println(merge(m3, m4))
  }
}
