package Inputs

class TicTacToeInput() extends Input[(Array[Int], String)] {
  var location: Array[Int] = new Array[Int](2)
  var player: String = ""

  override def setValue(input: String): Boolean = {
    val splitter: Array[String] = input.split(" ")
    if (splitter.length == 1) {
      return false
    } else {
      player = splitter(0).toUpperCase
      location(0) = 3 - (splitter(1).charAt(0).toInt - 48)
      location(1) = (splitter(1).charAt(1).toInt - 'a'.toInt)
    }
    true
  }

  override def getValue(): (Array[Int],String) = {
    (location, player)
  }
}