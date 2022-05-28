import Inputs.Input

import javax.swing.JFrame

/*
S : generic type for the State class
I : generic type for the input class
 */
class GameEngine[S, I](var state: S, var move: I) {
    def game(drawer: (JFrame, S) => Unit, controller: (S, I, Int) => (S, Boolean)): Unit = {
      var turn = 0
      var frame =new JFrame()
      drawer(frame,state)
      while (true) {
        val (newState, valid) = controller(state, move, turn)
        if (valid) {
          drawer(frame,newState)
          state = newState
          turn = (turn + 1) % 2
        }
        else {
          println("Invalid")
        }
      }
    }
}