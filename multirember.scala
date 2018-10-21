object App {
  def multiremberAndCo[A,B](a: A, lat: List[A], col: (List[A], List[A]) => B): B = lat match {
    case Nil => col(Nil, Nil)
    case h :: t if h == a => multiremberAndCo[A, B](a, t, (newlat,seen) => col(newlat, h :: seen))
    case h :: t => multiremberAndCo[A, B](a, t, (newlat, seen) => col(h :: newlat, seen))
  }

  def main(args: Array[String]): Unit = {
    multiremberAndCo[String, Unit]("tuna", List("tuna", "salad", "and", "more", "tuna"), (xs, ys) => println(s"***$xs\n$ys\n***"))
  }
}
