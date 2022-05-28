import Inputs.ChessInput
import States.ChessState

import scala.language.postfixOps
import scala.util.control.Breaks.{break, breakable}


class Chess extends Game[ChessState, ChessInput] {

  var cKing: Array[Int] = Array(0, 3)
  var sKing: Array[Int] = Array(7, 3)
  val printBoard: ChessState => Unit = (state: ChessState) => {
    println("   a   b   c   d   e   f   g   h")
    println("-|###|---|###|---|###|---|###|---|-")
    var index = 8
    for (i <- 0 to 22) {
      if (i % 3 != 0) {
        if (i % 2 == 0) print("-|---|###|---|###|---|###|---|###|-")
        else print("-|###|---|###|---|###|---|###|---|-")
      } else {
        print(index)
        //        index = index - 1;
        for (j <- 0 to 32) {
          if (j % 4 == 0) print("|")
          else {
            if ((j - 1) % 4 != 0 && (j + 1) % 4 != 0) print(state.getPieces()(i / 3)(j / 4))
            else print(" ")
          }
        }
        print(index)
        index = index - 1
      }
      println()
    }
    println("   a   b   c   d   e   f   g   h")

  }

  val checkPlay: (ChessState, ChessInput) => Boolean = (state: ChessState, newPlay: ChessInput) => {
    val rO = newPlay.prevIndex(0)
    val cO = newPlay.prevIndex(1)
    val rN = newPlay.newIndex(0)
    val cN = newPlay.newIndex(1)
    val piece = state.getPieces()(rO)(cO)
    if (state.player == "W") { //small
      if (piece(0).isUpper) {
        println("please play with white ")
        false
      } else {
        piece match {
          case "p" =>
            if (!pawn(state, rO, cO, rN, cN)) {
              false
            } else {
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.sKingCheck) {
                println("small king in check.")
                state.sKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else {
                true
              }
            }
          case "r" =>
            if (!Rook(state, rO, cO, rN, cN)) {
              false
            } else {
              if (rO == 7 && cO == 0) state.sKingLeftCastle = false;
              else if (rO == 7 && cO == 7) state.sKingRightCastle = false;
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.sKingCheck) {
                println("small king in check.")
                state.sKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else true
            }
          case "b" =>
            if (!Bishop(state, rO, cO, rN, cN)) {
              false
            } else {
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.sKingCheck) {
                println("small king in check.")
                state.sKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else true
            }
          case "h" =>
            if (!knight(state, rO, cO, rN, cN)) {
              false
            } else {
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.sKingCheck) {
                println("small king in check.")
                state.sKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else true
            }
          case "q" =>
            if (!Queen(state, rO, cO, rN, cN)) {
              false
            } else {
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.sKingCheck) {
                println("small king in check.")
                state.sKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else true
            }
          case "k" =>
            if (!King(state, rO, cO, rN, cN)) {
              false
            } else {
              state.sKingLeftCastle = false
              state.sKingRightCastle = false
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.sKingCheck) {
                println("small king in check")
                state.sKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else true
            }
          case _ => false
        }
      }
    } else if (state.player == "B") {
      if (piece(0).isLower) {
        println("please play with Black")
        false
      } else {
        piece match {
          case "P" =>
            if (!pawn(state, rO, cO, rN, cN)) {
              false
            } else {
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.cKingCheck) {
                println("capital king in check.")
                state.cKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else true
            }
          case "R" =>
            if (!Rook(state, rO, cO, rN, cN)) {
              false
            } else {
              if (rO == 0 && cO == 0) state.cKingLeftCastle = false;
              else if (rO == 7 && cO == 7) state.cKingRightCastle = false;
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.cKingCheck) {
                println("capital king in check.")
                state.cKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else true
            }
          case "B" =>
            if (!Bishop(state, rO, cO, rN, cN)) {
              false
            } else {
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.cKingCheck) {
                println("capital king in check")
                state.cKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else true
            }
          case "H" =>
            if (!knight(state, rO, cO, rN, cN)) {
              false
            } else {
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.cKingCheck) {
                println("capital king in check")
                state.cKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else true
            }
          case "Q" =>
            if (!Queen(state, rO, cO, rN, cN)) {
              false
            } else {
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.cKingCheck) {
                println("capital king in check")
                state.cKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else true
            }
          case "K" =>
            if (!King(state, rO, cO, rN, cN)) {
              false
            } else {
              state.cKingLeftCastle = false
              state.cKingRightCastle = false
              applyMove(state, rO, cO, rN, cN)
              checkKing(state)
              if (state.cKingCheck) {
                println("capital king in check.")
                state.cKingCheck = false
                applyMove(state, rN, cN, rO, cO)
                false
              } else true
            }
          case _ => false
        }
      }
    } else {
      false
    }
  }


  def EmptyPlacesHorizontal(state: ChessState, rOld: Int, rNew: Int, cNew: Int): Boolean = {
    var boo = false;
    var Big = 0;
    var small = 0
    if (rOld > rNew) {
      Big = rOld;
      small = rNew
    } else {
      small = rOld;
      Big = rNew
    }
    if (small < Big - 1) {
      for (i <- small + 1 to Big - 1) {
        if (state.getPieces()(i)(cNew) == " ") {
          boo = true
        }
      }
    } else {
      boo = true
    }
    boo
  }

  def EmptyPlacesVertical(state: ChessState, rNew: Int, cOld: Int, cNew: Int): Boolean = {
    var boo = false
    var Big = 0
    var small = 0
    if (cOld > cNew) {
      Big = cOld;
      small = cNew
    } else {
      small = cOld;
      Big = cNew
    }
    if (small < Big - 1) {
      for (i <- small + 1 to Big - 1) {
        if (state.getPieces()(rNew)(i) == " ") {
          boo = true
        }
      }
    } else {
      boo = true
    }
    boo
  }

  def EmptyPlacesDiagonal(state: ChessState, rOld: Int, rNew: Int, cOld: Int, cNew: Int): Boolean = {
    var boo: Boolean = true
    var rPlus: Int = 0
    var cPlus: Int = 0
    if (rOld < rNew && cOld < cNew) {
      rPlus = 1
      cPlus = 1
    } else if (rOld > rNew && cOld < cNew) {
      rPlus = -1
      cPlus = 1
    } else if (rOld > rNew && cOld > cNew) {
      rPlus = -1
      cPlus = -1
    } else {
      rPlus = 1
      cPlus = -1
    }
    var i = rOld + rPlus;
    var j = cOld + cPlus
    breakable {
      while (i != rNew) {
        if (state.getPieces()(i)(j) != " ") {
          boo = false
          break
        }
        i = i + rPlus;
        j = j + cPlus
      }
    }
    boo
  }

  def King: (ChessState, Int, Int, Int, Int) => Boolean = (state: ChessState, rOld: Int, cOld: Int, rNew: Int, cNew: Int) => {
    val New = state.getPieces()(rNew)(cNew)
    val Old = state.getPieces()(rOld)(cOld)
    if (((rOld - rNew).abs == 1 && (cOld - cNew).abs == 1) || ((rOld - rNew).abs == 1 && cNew == cOld) ||
      ((cOld - cNew).abs == 1 && rNew == rOld)) {
      if ((New == " ") || (Old(0).isUpper && New(0).isLower) || (New(0).isUpper && Old(0).isLower)) {
        if (state.getPieces()(rOld)(cOld)(0).isLower) {
          sKing(0) = rNew
          sKing(1) = cNew
        }
        else if (state.getPieces()(rOld)(cOld)(0).isUpper) {
          cKing(0) = rNew
          cKing(1) = cNew
        }
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  def checkKing(state: ChessState): Unit = {
    var i, j = 0
    breakable {
      for (i <- 0 to 7) {
        breakable {
          for (j <- 0 to 7) {
            val place = state.getPieces()(i)(j)
            if (state.getPieces()(i)(j)(0).isLower && !state.getPieces()(i)(j).isEmpty) {
              place match {
                case "r" =>
                  if (Rook(state, i, j, cKing(0), cKing(1))) {
                    state.cKingCheck = true
                    break
                  }
                case "h" =>
                  if (knight(state, i, j, cKing(0), cKing(1))) {
                    state.cKingCheck = true
                    break
                  }
                case "b" =>
                  if (Bishop(state, i, j, cKing(0), cKing(1))) {
                    state.cKingCheck = true
                    break
                  }
                case "q" =>
                  if (Queen(state, i, j, cKing(0), cKing(1))) {
                    state.cKingCheck = true
                    break
                  }
                case "p" =>
                  if (pawn(state, i, j, cKing(0), cKing(1))) {
                    state.cKingCheck = true
                    break
                  }
                case _ => break
              }
            }
            else if (state.getPieces()(i)(j)(0).isUpper && !state.getPieces()(i)(j).isEmpty) {
              place match {
                case "R" =>
                  if (Rook(state, i, j, sKing(0), sKing(1))) {
                    state.sKingCheck = true
                    break
                  }
                case "H" =>
                  if (knight(state, i, j, sKing(0), sKing(1))) {
                    state.sKingCheck = true
                    break
                  }
                case "B" =>
                  if (Bishop(state, i, j, sKing(0), sKing(1))) {
                    state.sKingCheck = true
                    break
                  }
                case "Q" =>
                  if (Queen(state, i, j, sKing(0), sKing(1))) {
                    state.sKingCheck = true
                    break
                  }
                case "P" =>
                  if (pawn(state, i, j, sKing(0), sKing(1))) {
                    state.sKingCheck = true
                    break
                  }
                case _ => break
              }
            }
          }
        }
        if ((state.cKingCheck == true) || (state.sKingCheck == true)) {
          break
        }
      }
    }
  }

  def Queen: (ChessState, Int, Int, Int, Int) => Boolean = (state: ChessState, rOld: Int, cOld: Int, rNew: Int, cNew: Int) => {
    val New = state.getPieces()(rNew)(cNew)
    val Old = state.getPieces()(rOld)(cOld)
    if (validMoveQueen(state, rOld, cOld, rNew, cNew) &&
      ((Old(0).isUpper && New(0).isLower) || (New(0).isUpper && Old(0).isLower) || (New == " "))) {
      true
    } else {
      false
    }
  }

  def validMoveQueen(state: ChessState, rOld: Int, cOld: Int, rNew: Int, cNew: Int): Boolean = {
    if (validMoveBishop(state, rOld, cOld, rNew, cNew) || validMoveRook(state, rOld, cOld, rNew, cNew)) {
      true
    } else {
      false
    }
  }

  def Rook: (ChessState, Int, Int, Int, Int) => Boolean = (state: ChessState, rOld: Int, cOld: Int, rNew: Int, cNew: Int) => {
    val New = state.getPieces()(rNew)(cNew)
    val Old = state.getPieces()(rOld)(cOld)
    if (validMoveRook(state, rOld, cOld, rNew, cNew)) {
      if (New == " ") {
        true
      } else if ((Old(0).isUpper && New(0).isLower) || (New(0).isUpper && Old(0).isLower)) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  def validMoveRook(state: ChessState, rOld: Int, cOld: Int, rNew: Int, cNew: Int): Boolean = {
    if ((rNew - rOld).abs <= 7 && cNew == cOld && EmptyPlacesHorizontal(state, rOld, rNew, cNew)) { //||
      true
    } else if ((cNew - cOld).abs <= 7 && rNew == rOld && EmptyPlacesVertical(state, rNew, cOld, cNew)) {
      true
    }
    else {
      false
    }
  }

  def Bishop(state: ChessState, rOld: Int, cOld: Int, rNew: Int, cNew: Int): Boolean = {
    if (validMoveBishop(state, rOld, cOld, rNew, cNew)) {
      if (state.getPieces()(rNew)(cNew) == " ") {
        true
      } else {
        if ((state.getPieces()(rNew)(cNew)(0).isUpper && state.getPieces()(rOld)(cOld)(0).isLower) ||
          (state.getPieces()(rNew)(cNew)(0).isLower && state.getPieces()(rOld)(cOld)(0).isUpper)) {
          true
        } else {
          false
        }
      }
    } else {
      false
    }
  }

  def validMoveBishop(state: ChessState, rOld: Int, cOld: Int, rNew: Int, cNew: Int): Boolean = {
    if ((rNew - rOld).abs == (cNew - cOld).abs && EmptyPlacesDiagonal(state, rOld, rNew, cOld, cNew)) {
      true
    } else {
      false
    }
  }

  def pawn(state: ChessState, rOld: Int, cOld: Int, rNew: Int, cNew: Int): Boolean = {
    var cProm = false
    var sProm = false
    // Capital Letters
    if (state.getPieces()(rOld)(cOld)(0).isUpper) {
      if (rNew - rOld == 2 && cOld == cNew && firstMovePawn(state, rOld, cOld)) {
        true
      }
      else if (rNew - rOld == 1 && (cOld == cNew || ((cOld - cNew).abs == 1 && state.getPieces()(rNew)(cNew)(0).isLower))) {
        if (rNew == 7) {
          cProm = true
          promotePawn(cProm, sProm, state, rOld, cOld, rNew, cNew)
          true
        }
        else {
          true
        }
      } else {
        false
      }
    }
    // Small
    else if (state.getPieces()(rOld)(cOld)(0).isLower) {
      if (rOld - rNew == 2 && cOld == cNew && firstMovePawn(state, rOld, cOld)) {
        true
      }
      else if (rOld - rNew == 1 && (cOld == cNew || (cOld - cNew).abs == 1 && state.getPieces()(rNew)(cNew)(0).isUpper)) {
        if (rNew == 0) {
          sProm = true
          promotePawn(cProm, sProm, state, rOld, cOld, rNew, cNew)
          true
        }
        else {
          true
        }
      } else {
        false
      }
    } else {
      false
    }
  }

  def firstMovePawn(state: ChessState, rOld: Int, cOld: Int): Boolean = {
    if (state.getPieces()(rOld)(cOld)(0).isUpper) {
      if (rOld == 1) {
        true
      } else {
        false
      }
    }
    else if (state.getPieces()(rOld)(cOld)(0).isLower) {
      if (rOld == 6) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  def promotePawn(capital: Boolean, small: Boolean, state: ChessState, rOld: Int, cOld: Int, rNew: Int, cNew: Int): Unit = {
    print("Enter Promotion Piece: ")
    val input: String = scala.io.StdIn.readLine()
    if (capital) {
      if (input(0).isUpper) {
        state.getPieces()(rNew)(cNew) = input
        state.getPieces()(rOld)(cOld) = " "
      }
    }
    if (small) {
      if (input(0).isLower) {
        state.getPieces()(rNew)(cNew) = input
        state.getPieces()(rOld)(cOld) = " "
      }
    }
  }

  def validMoveKnight(rOld: Int, cOld: Int, rNew: Int, cNew: Int): Boolean = {
    if ((rNew == rOld + 2) || (rNew == rOld - 2)) {
      if ((cNew == cOld + 1) || (cNew == cOld - 1)) {
        true
      } else {
        false
      }
    } else if ((cNew == cOld + 2) || (cNew == cOld - 2)) {
      if (rNew == cOld + 1 || rNew == rOld + 1) {
        true
      } else {
        false
      }
    } else {
      false
    }

  }

  def knight: (ChessState, Int, Int, Int, Int) => Boolean = (state: ChessState, rOld: Int, cOld: Int, rNew: Int, cNew: Int) => {
    if (validMoveKnight(rOld, cOld, rNew, cNew)) {
      if (state.getPieces()(rNew)(cNew) == " ") {
        true
      }
      else if (state.getPieces()(rOld)(cOld)(0).isLower && state.getPieces()(rNew)(cNew)(0).isUpper) {
        true
      }
      else if (state.getPieces()(rOld)(cOld)(0).isUpper && state.getPieces()(rNew)(cNew)(0).isLower) {
        true
      }
      else {
        false
      }
    } else {
      false
    }
  }

  def applyMove(state: ChessState, rOld: Int, cOld: Int, rNew: Int, cNew: Int): Unit = {
    state.getPieces()(rNew)(cNew) = state.getPieces()(rOld)(cOld)
    state.getPieces()(rOld)(cOld) = " "
  }

  val change: (ChessState, ChessInput) => ChessState = (state: ChessState, newPlay: ChessInput) => {
    if (state.player == "W") { //white
      println("it is turn of Black (capital) ")
      state.player = "B" //black
    } else {
      state.player = "W"
      println("it is turn of White (small)")
    }
    state
  }
  /*
    def checkCastleMove(state : ChessState,r0: Int, c0: Int, rN: Int, cN: Int,rookRow: Int, rookColumn: Int): Boolean = {
      var res : Boolean = true;
      var i : Int = r0

    }*/

  override def drawer(state: ChessState): Unit = {
    printBoard(state)
  }

  override def controller(state: ChessState, input: ChessInput, turn: Int): (ChessState, Boolean) = {
    println("Enter the next Move:")
    val nextMove: String = scala.io.StdIn.readLine()
    if (input.setValue(nextMove)) {
      if (checkPlay(state, input)) {
        return (change(state, input), true)
      }
    }
    return (state, false)
  }
}

