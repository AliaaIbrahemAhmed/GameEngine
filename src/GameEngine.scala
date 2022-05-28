import Inputs.Input

import javax.swing.JFrame

/*
S : generic type for the State class
T : generic type for the Turn class
I : generic type for the input class
 */
class GameEngine[S, I](var state: S, var move: I) {
  def game(drawer: (JFrame, S) => Unit,
           controller: (S, I, Int) => (S, Boolean)
          ): Unit = {
    var frame =new JFrame()
    var turn = 0
    drawer(frame,state)
    while (true) {
      println("Please Enter the next Move")
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