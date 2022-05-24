import States.CheckersState
import Turns.CheckersTurn

class Checkers extends Game[CheckersState, CheckersTurn] {
  override val printBoard: CheckersState => Unit = (state: CheckersState) => {
    println("|###|---|###|---|###|---|###|---|")
    for (i <- 0 to 22) {
      if (i % 3 != 0) {
        if (i % 2 == 0) print("|---|###|---|###|---|###|---|###|")
        else print("|###|---|###|---|###|---|###|---|")
      } else {
        for (j <- 0 to 32) {
          if (j % 4 == 0) print("|")
          else {
            if ((j - 1) % 4 != 0 && (j + 1) % 4 != 0) print(state.getPieces()(i / 3)(j / 4))
            else print(" ")
          }
        }
      }
      println()
    }
  }


  override val parseInput: String => CheckersTurn = (input: String) => {
    var index = Array(input.indexOf(0), input.indexOf(0))
    var newTurn: CheckersTurn = new CheckersTurn(index, index, true, true)
    println("Input is parsed")
    newTurn
  }

  override val checkPlay: (CheckersState, CheckersTurn) => Boolean = (pieces: CheckersState, newPlay: CheckersTurn) => {
    println("Input is Checked")
    true
  }

  def getPlayer(player: Boolean): String = {
    if (player) return "W"
    "B"
  }

  override val change: (CheckersState, CheckersTurn) => CheckersState = (pieces: CheckersState, newPlay: CheckersTurn) => {
    //  pieces(newPlay.index(0))(newPlay.index(1)) =  getPlayer(newPlay.player)
    println("Input is Changed")
    pieces
  }
  override val initialState: CheckersState = new CheckersState(Array(
    Array(" ", "B", " ", "B", " ", "B", " ", "B"),
    Array("B", " ", "B", " ", "B", " ", "B", " "),
    Array(" ", "B", " ", "B", " ", "B", " ", "B"),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array("W", " ", "W", " ", "W", " ", "W", " "),
    Array(" ", "W", " ", "W", " ", "W", " ", "W"),
    Array("W", " ", "W", " ", "W", " ", "W", " ")))
}
