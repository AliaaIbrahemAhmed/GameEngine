import States.TicTacToeState
import Turns.TicTacToeTurn
import Inputs.TicTacToeInput
class TicTacToe  {
  def drawer(TicTacToeState: TicTacToeState): Unit = {
      println("      a         b          c")

      println("   ______________________________")
      println("3 |   " + TicTacToeState.board(0)(0) + "    |    " + TicTacToeState.board(0)(1) + "     |    " + TicTacToeState.board(0)(2) + "    |")
      println("  _______________________________")
      println("2 |   " + TicTacToeState.board(1)(0) + "    |    " + TicTacToeState.board(1)(1) + "     |    " + TicTacToeState.board(1)(2) + "    |")
      println("  _______________________________")
      println("1 |   " + TicTacToeState.board(2)(0) + "    |    " + TicTacToeState.board(2)(1) + "     |    " + TicTacToeState.board(2)(2) + "    |")
      println("  _______________________________")
      println("      a         b          c")
    }
  def controller(ticTacToeState: TicTacToeState, input: TicTacToeInput, turn: Int): (TicTacToeState, Boolean) = {
    val PlayerAndMove: String = scala.io.StdIn.readLine()
    if(input.setValue(PlayerAndMove)) {
      var spliter = new Array[String](2)
      spliter = PlayerAndMove.split(" ")
      var player = spliter(0).toUpperCase
      var location = new Array[Int](2)
      location(0) = 3 - (spliter(1).charAt(0).toInt - 48)
      location(1) = (spliter(1).charAt(1).toInt - 'a'.toInt)
      var row = location(0)
      var col = location(1)
      if (row >= 3 || col >= 3 ||row<0 || col<0  || (!player.equals("X") && !player.equals("O"))) {
        (ticTacToeState,false)
      }
      else if(turn ==0&& player.equals("O")){
        (ticTacToeState,false)
      }
      else if(turn==1&&player.equals("X")){
        (ticTacToeState,false)

      }
      else if (ticTacToeState.board(row)(col) == " ") {
        if(turn==0) {
          ticTacToeState.board(row)(col) = "X"

        }
        else {
          ticTacToeState.board(row)(col) = "O"
        }
        (ticTacToeState,true)
      }
    else {
        (ticTacToeState,false)
    }
  }
    else {
      (ticTacToeState, false)
    }


}
}


