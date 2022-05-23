object Main {
  def main(args: Array[String]): Unit = {
    var x: Checkers = new Checkers
    var y: Array[String] = Array("1", "2", "3")
    var gameEngine: GameEngine[Array[Array[String]],Turn] = new GameEngine[Array[Array[String]],Turn](x.initialState)
    gameEngine.game(x.printBoard, x.parseInput, x.checkPlay, x.change)
  }
}
