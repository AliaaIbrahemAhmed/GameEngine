package Inputs

class ChessInput() extends Input[(Array[Int], Array[Int])] {
  var prevIndex: Array[Int] = new Array[Int](2)
  var newIndex: Array[Int] = new Array[Int](2)


  override def setValue(input : String): Boolean = {
    val input1 = input.replaceAll("\\s+", "")
    if (input1.length != 4 || !input1.charAt(0).isLetter || !input1.charAt(2).isLetter
      || !input1.charAt(1).isDigit || !input1.charAt(3).isDigit || input1.charAt(0).toLower > 'h' || input1.charAt(2).toLower > 'h' ||
      input1.charAt(1) - '0' < 1 || input1.charAt(1) - '0' > 8 || input1.charAt(3) - '0' < 1 || input1.charAt(3) - '0' > 8) {
      false
    }
    else {
      newIndex = Array(Math.abs('8' - input1.charAt(3)), Math.abs('a' - input1.charAt(2).toLower))
      prevIndex = Array(Math.abs('8' - input1.charAt(1)), Math.abs('a' - input1.charAt(0).toLower))
      true
    }
  }

  override def getValue(): (Array[Int], Array[Int]) = {
    (prevIndex, newIndex)
  }
}
