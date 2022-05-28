package Inputs

trait Input[k] {
  def setValue(input : String): Boolean
  def getValue(): k

}
