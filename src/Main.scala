import States.CheckersState
import Turns.CheckersTurn
import States.TicTacToeState
import Turns.TicTacToeTurn
object Main {
  def main(args: Array[String]): Unit = {
    var x: TicTacToe = new TicTacToe
    var y: Array[String] = Array("1", "2", "3")
    var gameEngine: GameEngine[TicTacToeState,TicTacToeTurn] = new GameEngine[TicTacToeState,TicTacToeTurn](x.initialState)
    gameEngine.game(x.printBoard, x.parseInput, x.checkPlay, x.change)
  }
}
