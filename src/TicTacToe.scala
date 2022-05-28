import States.TicTacToeState
import Inputs.TicTacToeInput

import java.awt.image.BufferedImage
import java.awt.{Color, Font, Graphics, Graphics2D}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.{JFrame, JPanel}
  class TicTacToe extends Game[TicTacToeState, TicTacToeInput] {
    def drawer(frame: JFrame, TicTacToeState: TicTacToeState): Unit = {
      println("      a         b          c")
      println("   ______________________________")
      println("3 |   " + TicTacToeState.board(0)(0) + "    |    " + TicTacToeState.board(0)(1) + "     |    " + TicTacToeState.board(0)(2) + "    |")
      println("  _______________________________")
      println("2 |   " + TicTacToeState.board(1)(0) + "    |    " + TicTacToeState.board(1)(1) + "     |    " + TicTacToeState.board(1)(2) + "    |")
      println("  _______________________________")
      println("1 |   " + TicTacToeState.board(2)(0) + "    |    " + TicTacToeState.board(2)(1) + "     |    " + TicTacToeState.board(2)(2) + "    |")
      println("  _______________________________")
      println("      a         b          c")
      draw(frame, TicTacToeState.board)
    }

    def controller(ticTacToeState: TicTacToeState, input: TicTacToeInput, turn: Int): (TicTacToeState, Boolean) = {
      println("Enter the next Move:")
      val nextMove: String = scala.io.StdIn.readLine()
      if (input.setValue(nextMove)) {
        val (location, player) = input.getValue()
        val row = location(0)
        val col = location(1)
        if (row >= 3 || col >= 3 || row < 0 || col < 0 || (!player.equals("X") && !player.equals("O"))) {
          (ticTacToeState, false)
        }
        else if (turn == 0 && player.equals("O")) {
          (ticTacToeState, false)
        }
        else if (turn == 1 && player.equals("X")) {
          (ticTacToeState, false)

        }
        else if (ticTacToeState.board(row)(col) == " ") {
          if (turn == 0) {
            ticTacToeState.board(row)(col) = "X"

          }
          else {
            ticTacToeState.board(row)(col) = "O"
          }
          (ticTacToeState, true)
        }
        else {
          (ticTacToeState, false)
        }
      }
      else {
        (ticTacToeState, false)
      }

    }

    def draw(frame: JFrame, board: Array[Array[String]]) {
      frame.getContentPane().removeAll()
      frame.setBounds(10, 10, 512, 512);
      val imgx: BufferedImage = ImageIO.read(new File("x.png"))
      val imgo: BufferedImage = ImageIO.read(new File("o.png"))
      var x = imgx.getScaledInstance(64, 64, BufferedImage.TYPE_INT_ARGB)
      var o = imgo.getScaledInstance(64, 64, BufferedImage.TYPE_INT_ARGB)
      var pn = new JPanel(null) {
        override def paint(graphics: Graphics): Unit = {
          graphics.setFont(new Font("TimesRoman", Font.PLAIN, 30))
          for (i <- 0 to 2) {
            for (j <- 0 to 2) {
              graphics.setColor(Color.BLACK)
              graphics.drawRect(j * 64, i * 64, 64, 64)
              if(board(i)(j).equals("X")){
                graphics.drawImage(x, j * 64, i * 64, this)
              }
              else if(board(i)(j).equals("O")){
                graphics.drawImage(o, j * 64, i * 64, this)
              }
            }
          }
        }
      }
      frame.add(pn)
      frame.setDefaultCloseOperation(3);
      frame.setVisible(true);
    }
  }





