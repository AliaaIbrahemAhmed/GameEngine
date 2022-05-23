class Checkers extends Game[Array[Array[String]],Turn] {
  override val printBoard: Array[Array[String]] => Unit = (pieces: Array[Array[String]]) => {
    println("Board is printed!")
  }

  override val parseInput: String => Turn = (input: String) => {
    var index = Array(input.indexOf(0), input.indexOf(0))
    var newTurn: Turn = new Turn(index, true, true) 
    println("Input is parsed")
    newTurn 
  }

  override val checkPlay: (Array[Array[String]], Turn) => Boolean = (pieces: Array[Array[String]], newPlay: Turn) => {
    println("Input is Checked")
    true 
  }

  def getPlayer(player: Boolean): String = {
    if (player) return "W"
    "B"
  }

  override val change: (Array[Array[String]], Turn) => Array[Array[String]] = (pieces: Array[Array[String]], newPlay: Turn) => {
    pieces(newPlay.index(0))(newPlay.index(1)) =  getPlayer(newPlay.player)
    println("Input is Changed")
    pieces 
  }
  override val initialState: Array[Array[String]] = Array(
    Array(" ", "B", " ", "B", " ", "B", " ", "B"),
    Array("B", " ", "B", " ", "B", " ", "B", " "),
    Array(" ", "B", " ", "B", " ", "B", " ", "B"),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array(" ", " ", " ", " ", " ", " ", " ", " "),
    Array("W", " ", "W", " ", "W", " ", "W", " "),
    Array(" ", "W", " ", "W", " ", "W", " ", "W"),
    Array("W", " ", "W", " ", "W", " ", "W", " "))
}
