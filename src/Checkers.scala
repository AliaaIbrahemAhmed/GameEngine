import Inputs.CheckersInput
import States.CheckersState

class Checkers extends Game[CheckersState, CheckersInput] {

  def drawer(state: CheckersState): Unit = {
    printBoard(state)
  }

  def controller(state: CheckersState, input: CheckersInput, turn: Int): (CheckersState, Boolean) = {
    println("Enter the next Move:")
    val nextMove: String = scala.io.StdIn.readLine()
    if (input.setValue(nextMove)) {
      if (checkPlay(state, input)) {
        return (change(state, input), true)
      }
    }
    (state, false)
  }

  val printBoard: CheckersState => Unit = (state: CheckersState) => {
    println("   A   B   C   D   E   F   G   H  ")
    println(" |###|---|###|---|###|---|###|---|")
    var counter = 0
    for (i <- 0 to 22) {
      if (i % 3 != 0) {
        if (i % 2 == 0) print(" |---|###|---|###|---|###|---|###|")
        else print(" |###|---|###|---|###|---|###|---|")
      } else {
        for (j <- 0 to 32) {
          if (j == 0) {
            print(8 - counter)
            counter += 1
          }
          if (j % 4 == 0) print("|")
          else {
            if ((j - 1) % 4 != 0 && (j + 1) % 4 != 0) print(state.getPieces(i / 3)(j / 4))
            else print(" ")
          }
        }
      }
      println()
    }
    println("   A   B   C   D   E   F   G   H  ")
  }


  def checkForwardMove(newPlay: CheckersInput): Boolean = {
    (newPlay.prevIndex(0) == (newPlay.newIndex(0) + 1)) && (Math.abs(newPlay.prevIndex(1) - newPlay.newIndex(1)) == 1)
  }

  def checkBackwardMove(newPlay: CheckersInput): Boolean = {
    (newPlay.prevIndex(0) == (newPlay.newIndex(0) - 1)) && (Math.abs(newPlay.prevIndex(1) - newPlay.newIndex(1)) == 1)
  }

  def validMove(x: Int, y: Int): Boolean = {
    x >= 0 && y >= 0 && x <= 7 && y <= 7
  }

  def getPlayer(player: String): Boolean = {
    player.equals("w")
  }

  def getTurns(currIndex: Array[Int], validCaptures: Vector[Array[Int]]): Vector[(Array[Int], Array[Int])] = {
    var validTurns: Vector[(Array[Int], Array[Int])] = Vector.empty[(Array[Int], Array[Int])]
    var i = 0;
    while (i < validCaptures.length) {
      validTurns = validTurns.appended((validCaptures(i), currIndex))
      i += 1
    }
    validTurns
  }

  def checkCapture(x: Int, y: Int, state: CheckersState, player: String): Vector[Array[Int]] = {
    var res: Vector[Array[Int]] = Vector.empty[Array[Int]]
    val isKing: Boolean = state.isKing(Array(x, y))
    if ((validMove(x + 1, y + 1) && validMove(x + 2, y + 2)) && (isKing || player.toLowerCase().equals("b")) && (!state.getPieces(x + 1)(y + 1).equals(" ")) && getPlayer(state.getPieces(x + 1)(y + 1)) == !getPlayer(player) && state.getPieces(x + 2)(y + 2).equals(" ")) {
      res = res.appended(Array(x + 2, y + 2))
    }
    if ((validMove(x + 1, y - 1) && validMove(x + 2, y - 2)) && (isKing || player.toLowerCase().equals("b")) && (!state.getPieces(x + 1)(y - 1).equals(" ")) && getPlayer(state.getPieces(x + 1)(y - 1)) == !getPlayer(player) && state.getPieces(x + 2)(y - 2).equals(" ")) {
      res = res.appended(Array(x + 2, y - 2))
    }
    if ((validMove(x - 1, y + 1) && validMove(x - 2, y + 2)) && (isKing || player.toLowerCase().equals("w")) && (!state.getPieces(x - 1)(y + 1).equals(" ")) && getPlayer(state.getPieces(x - 1)(y + 1)) == !getPlayer(player) && state.getPieces(x - 2)(y + 2).equals(" ")) {
      res = res.appended(Array(x - 2, y + 2))
    }
    if ((validMove(x - 1, y - 1) && validMove(x - 2, y - 2)) && (isKing || player.toLowerCase().equals("w")) && (!state.getPieces(x - 1)(y - 1).equals(" ")) && getPlayer(state.getPieces(x - 1)(y - 1)) == !getPlayer(player) && state.getPieces(x - 2)(y - 2).equals(" ")) {
      res = res.appended(Array(x - 2, y - 2))
    }
    res
  }

  def getValidCaptureMoves(state: CheckersState, player: String): Vector[(Array[Int], Array[Int])] = {
    var validCaptureTurns: Vector[(Array[Int], Array[Int])] = Vector.empty[(Array[Int], Array[Int])]
    var i = 0
    var j = 0
    while (i <= 7) {
      j = 0
      while (j <= 7) {
        if (state.getPieces(i)(j).toLowerCase() == player) {
          val x: Vector[Array[Int]] = checkCapture(i, j, state, player)
          if (x != null) validCaptureTurns ++= getTurns(Array(i, j), x)
        }
        j += 1
      }
      i += 1
    }
    validCaptureTurns
  }

  def getCurrPiece(player: Boolean): String = {
    if (player) return "w"
    "b"
  }

  def contain(newPlay: CheckersInput, validCaptureTurns: Vector[(Array[Int], Array[Int])]): Boolean = {
    for (i <- validCaptureTurns) {
      if (i._1.sameElements(newPlay.newIndex) && i._2.sameElements(newPlay.prevIndex)) return true
    }
    false
  }

  def checkCapture(state: CheckersState, newPlay: CheckersInput): Int = {
    val validCaptureTurns = getValidCaptureMoves(state, getCurrPiece(!state.getLastPlayer).toLowerCase())
    if (contain(newPlay, validCaptureTurns)) return 1
    else if (validCaptureTurns.isEmpty) return -1
    0
  }

  def checkNormalMove(state: CheckersState, newPlay: CheckersInput): Boolean = {
    val isKing = state.isKing(newPlay.prevIndex)
    state.getPieces(newPlay.newIndex(0))(newPlay.newIndex(1)).equals(" ") &&
      ((!state.getLastPlayer || isKing) && checkForwardMove(newPlay)) ||
      ((state.getLastPlayer || isKing) && checkBackwardMove(newPlay))
  }


  val checkPlay: (CheckersState, CheckersInput) => Boolean = (state: CheckersState, newPlay: CheckersInput) => {
    var flag = false
    val currPiece = state.getPieces(newPlay.prevIndex(0))(newPlay.prevIndex(1)).toLowerCase()
    if (currPiece == "w" && !state.getLastPlayer || currPiece == "b" && state.getLastPlayer) {
      if (checkCapture(state, newPlay) == 1) {
        state.setCapture()
        flag = true
      }
      else if (checkCapture(state, newPlay) == -1) {
        flag = checkNormalMove(state, newPlay)
      }
    }
    flag
  }

  def getInBetweenIndex(turn: CheckersInput): Array[Int] = {
    var res: Array[Int] = Array.ofDim[Int](2)
    var x = turn.prevIndex(0);
    var newX = turn.newIndex(0);
    var y = turn.prevIndex(1);
    var newY = turn.newIndex(1);
    if (x > newX && y > newY) {
      res = Array(x - 1, y - 1)
    } else if (x > newX && y < newY) {
      res = Array(x - 1, y + 1)
    } else if (x < newX && y > newY) {
      res = Array(x + 1, y - 1)
    } else if (x < newX && y < newY) {
      res = Array(x + 1, y + 1)
    }
    res
  }


  val change: (CheckersState, CheckersInput) => CheckersState = (state: CheckersState, newPlay: CheckersInput) => {
    val currPiece: String = state.getPieces(newPlay.prevIndex(0))(newPlay.prevIndex(1))
    state.toggleLastPlayer()
    if (currPiece == "w" && newPlay.newIndex(0) == 0) {
      state.getPieces(newPlay.newIndex(0))(newPlay.newIndex(1)) = "W"
      state.setKing(newPlay.newIndex)
    } else if (currPiece == "b" && newPlay.newIndex(0) == 7) {
      state.getPieces(newPlay.newIndex(0))(newPlay.newIndex(1)) = "B"
      state.setKing(newPlay.newIndex)
    } else {
      state.getPieces(newPlay.newIndex(0))(newPlay.newIndex(1)) = state.getPieces(newPlay.prevIndex(0))(newPlay.prevIndex(1))
      state.adjustKing(newPlay.newIndex, state.isKing(newPlay.prevIndex))
      state.reSetKing(newPlay.prevIndex)
    }
    if (state.hasCapture) {
      val x = getInBetweenIndex(newPlay)
      state.getPieces(x(0))(x(1)) = " "
      state.reSetCapture()
    }
    state.getPieces(newPlay.prevIndex(0))(newPlay.prevIndex(1)) = " "
    println("Input is Changed")
    state
  }
}
