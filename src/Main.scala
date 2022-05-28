import Inputs.{ConnectFourInput, TicTacToeInput}
import States.{CheckersState, ConnectFourState, TicTacToeState}
import Turns.CheckersTurn
import Turns.TicTacToeTurn

import javax.swing.JFrame
object Main {
  def main(args: Array[String]): Unit = {
//    var x: TicTacToe = new TicTacToe
//    var y: Array[String] = Array("1", "2", "3")
//    var gameEngine: GameEngine[TicTacToeState,TicTacToeTurn] = new GameEngine[TicTacToeState,TicTacToeTurn](x.initialState)
//    gameEngine.game(x.printBoard, x.parseInput, x.checkPlay, x.change)
    var valid = false
    while(!valid) {
      valid = true
      println("Enter the Number of the Game you want to play:")
      println("1. Tic-Tac-Toe")
      println("2. Connect-4")
      println("3. Checkers")
      println("4. Chess")
      val chosenGame = scala.io.StdIn.readInt()
      if (chosenGame == 1) {
        val gameEngine = new GameEngine[TicTacToeState, TicTacToeInput](new TicTacToeState(Array(
          Array(" ", " ", " "),
          Array(" ", " ", " "),
          Array(" ", " ", " "))), new TicTacToeInput)
        val TicTacToe = new TicTacToe
        gameEngine.game(TicTacToe.drawer, TicTacToe.controller)
      }
      else if (chosenGame == 2) {
        val gameEngine = new GameEngine[ConnectFourState, ConnectFourInput](new ConnectFourState, new ConnectFourInput)
        val connect4 = new ConnectFour
        gameEngine.game(connect4.drawer, connect4.controller)
      }
      else if (chosenGame == 3) {

      }
      else if (chosenGame == 4) {

      }
      else {
        println("Invalid Choice")
        valid = false
      }
    }
  }
}
