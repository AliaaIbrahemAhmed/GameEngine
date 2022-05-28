package Inputs

class ConnectFourInput() extends Input[Char]{
  var col: Char = '-'

  override def setValue(input : String): Boolean = {
    if(input.length == 1) {
      col = input.charAt(0)
      true
    }
    else false
  }

  def getValue: Char = col

}
