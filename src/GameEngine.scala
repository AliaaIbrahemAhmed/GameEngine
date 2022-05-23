/*
S : generic type for the State class
T : generic type for the Turn class
 */
class GameEngine[S, T](var state: S) {
  def game(printBoard: S => Unit,
           parseInput: String => T,
           checkPlay: (S, T) => Boolean,
           change: (S, T) => S): Unit = {
    while (true) {
      printBoard(state)
      print("Please Enter the next Move")
      var input: String = scala.io.StdIn.readLine() 
      var turn: T = parseInput(input)
      if (checkPlay(state, turn)) {
        state = change(state, turn) 
      }
    }
  }
}
