import Inputs.ConnectFourInput
import States.ConnectFourState

class ConnectFour extends Game[ConnectFourState, ConnectFourInput] {
  def drawer(connectState: ConnectFourState): Unit = {
    println(" a  b  c  d  e  f  g")
    connectState.state.foreach(row => {
      row.foreach(x => print(s" $x "))
      println()
    })
  }

  def controller(connectState: ConnectFourState, input: ConnectFourInput, turn: Int): (ConnectFourState, Boolean) = {
    println("Enter the next Move:")
    val nextMove: String = scala.io.StdIn.readLine()
    if (input.setValue(nextMove)) {
      val col = input.getValue - 'a'
      if (col < 0 || col > 6 || connectState.state(0)(col) != 'O') (connectState, false)
      else {
        var row = 5
        var empty = false
        while (!empty) {
          if (connectState.state(row)(col) == 'O') {
            if (turn == 0) connectState.state(row)(col) = 'R'
            else connectState.state(row)(col) = 'Y'
            empty = true
          }
          else row = row - 1
        }
        return (connectState, true)
      }
    }
    (connectState, false)
  }

}
