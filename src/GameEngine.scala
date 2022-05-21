class GameEngine[T](var state: T) {
  def game(printBoard: T => Unit,
           parseInput: String => Turn,
           checkPlay: (T, Turn) => Boolean,
           change: (T, Turn) => T): Unit = {
    while (true) {
      printBoard(state)
      print("Please Enter the next Move")
      var input: String = scala.io.StdIn.readLine();
      var turn: Turn = parseInput(input)
      if (turn.isValid && checkPlay(state, turn)) {
        state = change(state, turn);
      }
    }
  }
}
