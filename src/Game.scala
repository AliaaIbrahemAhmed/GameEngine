trait Game[S, T] {
  val printBoard: S => Unit
  val parseInput: String => T
  val checkPlay: (S, T) => Boolean
  val change: (S, T) => S
  val initialState: S
}
