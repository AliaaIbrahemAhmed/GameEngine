class TicTacToe extends Game[TicTacToeState,TicTacToeTurn] {
  override val printBoard: TicTacToeState => Unit = (state: TicTacToeState) => {
    println("      a         b          c")

    println("   ______________________________")
    println("3 |   " + state.board(0)(0) + "    |    " + state.board(0)(1) + "     |    " + state.board(0)(2) + "    |")
    println("  _______________________________")
    println("2 |   " + state.board(1)(0) + "    |    " + state.board(1)(1) + "     |    " + state.board(1)(2) + "    |")
    println("  _______________________________")
    println("1 |   " + state.board(2)(0) + "    |    " + state.board(2)(1) + "     |    " + state.board(2)(2) + "    |")
    println("  _______________________________")
    println("      a         b          c")
  }
  override val parseInput: String => TicTacToeTurn = (input: String) => {
    var spliter = new Array[String](2)
    spliter = input.split(" ")
    var player = spliter(0).toUpperCase
    var location = new Array[Int](2)
    location(0) = 3 - (spliter(1).charAt(0).toInt - 48)
    location(1) = (spliter(1).charAt(1).toInt - 'a'.toInt)
    var newTurn: TicTacToeTurn = new TicTacToeTurn(location, player)
    newTurn

  }
  override val checkPlay: (TicTacToeState, TicTacToeTurn) => Boolean = (state: TicTacToeState, newPlay: TicTacToeTurn) => {
    var row = newPlay.index(0)
    var col = newPlay.index(1)
    if (row >= 3 || col >= 3 ||state.player==newPlay.player || !newPlay.player.equals("X") && !newPlay.player.equals("O")) {
      false
    }
   else if (state.board(row)(col) ==" ") {
        state.board(row)(col) = newPlay.player
        true
    }
      else {
        false
      }
  }
  override val change: (TicTacToeState, TicTacToeTurn) => TicTacToeState = (state: TicTacToeState, newPlay: TicTacToeTurn) => {
    if(state.player=="X"){
      state.player="O"
    }
    else {
      state.player="X"
    }
    state
  }
  override val initialState:TicTacToeState = {
    var state=new TicTacToeState(Array(
      Array(" ", " ", " "),
      Array(" ", " ", " "),
      Array(" ", " ", " "))," ")
     state
  }

}

