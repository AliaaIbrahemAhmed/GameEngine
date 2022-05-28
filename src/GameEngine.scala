/*
S : generic type for the State class
I : generic type for the input class
 */
class GameEngine[S, I](var state: S, var move: I) {
  def game(drawer: S => Unit, controller: (S, I, Int) => (S, Boolean)): Unit = {
    var turn = 0
    drawer(state)
    while (true) {
      val (newState, valid) = controller(state, move, turn)
      if (valid) {
        drawer(newState)
        state = newState
        turn = (turn + 1) % 2
      }
      else {
        println("Invalid")
      }
    }
  }
}