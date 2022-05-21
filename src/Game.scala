trait Game[T] {
  val printBoard: T => Unit
  val parseInput: String => Turn
  val checkPlay: (T, Turn) => Boolean
  val change: (T, Turn) => T
}
