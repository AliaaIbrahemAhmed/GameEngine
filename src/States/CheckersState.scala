package States

class CheckersState() {
  var pieces: Array[Array[String]] = Array(
    Array(" ", "b", " ", "b", " ", "b", " ", "b"),
    Array("b", " ", "b", " ", "b", " ", "b", " "),
    Array(" ", "b", " ", "b", " ", "b", " ", "b"),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array("w", " ", "w", " ", "w", " ", "w", " "),
    Array(" ", "w", " ", "w", " ", "w", " ", "w"),
    Array("w", " ", "w", " ", "w", " ", "w", " "))
  var kings: Array[Array[Boolean]] = Array.ofDim[Boolean](8, 8)
  var lastPlayer: Boolean = false
  var lastPlayHasCapture: Boolean = false

  def getPieces: Array[Array[String]] = {
    pieces
  }

  def setPieces(newPieces: Array[Array[String]]): Unit = {
    pieces = newPieces
  }

  def isKing(index: Array[Int]): Boolean = {
    kings(index(0))(index(1))
  }

  def setKing(index: Array[Int]): Unit = {
    kings(index(0))(index(1)) = true
  }

  def reSetKing(index: Array[Int]): Unit = {
    kings(index(0))(index(1)) = false
  }

  def adjustKing(index: Array[Int], boolean: Boolean): Unit = {
    kings(index(0))(index(1)) = boolean
  }

  def getLastPlayer: Boolean = {
    lastPlayer
  }

  def toggleLastPlayer(): Unit = {
    lastPlayer = !lastPlayer
  }

  def hasCapture: Boolean = {
    lastPlayHasCapture
  }

  def setCapture(): Unit = {
    lastPlayHasCapture = true
  }

  def reSetCapture(): Unit = {
    lastPlayHasCapture = false
  }
}
