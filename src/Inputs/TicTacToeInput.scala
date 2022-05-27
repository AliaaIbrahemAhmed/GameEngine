package Inputs
class TicTacToeInput extends Input{
  override def setValue(input: String): Boolean = {
    var spliter = new Array[String](2)
    spliter = input.split(" ")
    if (spliter.length == 1) {
      return false
    }
    return true;
  }
}
