/*
S : generic type for the State class
T : generic type for the Turn class
 */
class GameEngine[S, T](var state: S) {
  def game(printBoard: S => Unit,
           parseInput: String => T,
           checkPlay: (S, T) => Boolean,
           change: (S, T) => S): Unit = {
    printBoard(state)
    while (true) {
      println("Please Enter the next Move")
      var input: String = scala.io.StdIn.readLine()
      var turn: T = parseInput(input)
      var valid=checkPlay(state,turn)
      if (valid) {
        printBoard(state)
        state = change(state, turn)
      }
      else {
        println("Invalid")
      }

    }
  }
}
