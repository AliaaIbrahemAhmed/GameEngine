class Checkers extends Game[Array[String]] {
  override val printBoard: Array[String] => Unit = (pieces: Array[String]) => {
    println("Board is printed!");
  }

  override val parseInput: String => Turn = (input: String) => {
    var index = Array(input.indexOf(0), input.indexOf(0))
    var newTurn: Turn = new Turn(index, true,true);
    println("Input is parsed")
    newTurn;
  }

  override val checkPlay: (Array[String], Turn) => Boolean = (pieces: Array[String], newPlay: Turn) => {
    println("Input is Checked")
    true;
  }

  override val change: (Array[String], Turn) => Array[String] = (pieces: Array[String], newPlay: Turn) => {
    println("Input is Changed")
    pieces;
  }
}
