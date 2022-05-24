package States

class CheckersState(private var pieces: Array[Array[String]],private var kings: Array[Array[Boolean]]) {
  def getPieces(): Array[Array[String]] = {
    pieces
  }

  def setPieces(newPieces: Array[Array[String]]): Unit = {
    pieces = newPieces
  }

  def isKing(index : Array[Int]): Boolean = {
    kings(index(0))(index(1))
  }

  def setKing(index : Array[Int]): Unit = {
    kings(index(0))(index(1)) = true
  }
}
