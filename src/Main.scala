import Inputs.{CheckersInput, ChessInput, ConnectFourInput, TicTacToeInput}
import States.{CheckersState, ChessState, ConnectFourState, TicTacToeState}

object Main {
  def main(args: Array[String]): Unit = {
    var valid = false
    while (!valid) {
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
        val gameEngine = new GameEngine[CheckersState, CheckersInput](new CheckersState, new CheckersInput);
        val checkers = new Checkers
        gameEngine.game(checkers.drawer, checkers.controller)
      }
      else if (chosenGame == 4) {
        val gameEngine = new GameEngine[ChessState, ChessInput](new ChessState, new ChessInput);
        val chess = new Chess
        gameEngine.game(chess.drawer, chess.controller)
      }
      else {
        println("Invalid Choice")
        valid = false
      }
    }
  }
}
