package States

class ChessState() {

  var pieces: Array[Array[String]] = Array(
    Array("R", "H", "B", "K", "Q", "B", "H", "R"),
    Array("P", "P", "P", "P", "P", "P", "P", "P"),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array("p", "p", "p", "p", "p", "p", "p", "p"),
    Array("r", "h", "b", "k", "q", "b", "h", "r"))
  var player: String = "W"
  var cKingCheck: Boolean = false
  var sKingCheck: Boolean = false
  var cKingLeftCastle: Boolean = true
  var cKingRightCastle: Boolean = true
  var sKingRightCastle: Boolean = true
  var sKingLeftCastle: Boolean = true

  def getPieces(): Array[Array[String]] = {
    pieces
  }

  def setPieces(newPieces: Array[Array[String]]): Unit = {
    pieces = newPieces
  }

  def getPlayer(): String = {
    player
  }

}
/*package Turns

class ChessTurn (var newIndex: Array[Char],var prevIndex: Array[Char], var isValid : Boolean) {

}
*/