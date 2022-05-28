import Inputs.ConnectFourInput
import States.ConnectFourState

import java.awt.{Color, Graphics, Image}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.{JFrame, JLabel, JPanel, JTextField}

class ConnectFour {
  def drawer(frame: JFrame, connectState: ConnectFourState): Unit = {
    println(" a  b  c  d  e  f  g")
    connectState.state.foreach(row => {
      row.foreach(x => print(s" $x "))
      println()
    })
    draw(frame,connectState.state)
  }

  def controller(connectState: ConnectFourState, input: ConnectFourInput, turn: Int): (ConnectFourState, Boolean) = {
    val move: String = scala.io.StdIn.readLine()
    input.setValue(move)
    val col = input.getValue - 'a'
    if(col < 0 || col > 6 || connectState.state(0)(col) != 'O') (connectState, false)
    else {
      var row = 5
      var empty = false
      while(!empty) {
        if(connectState.state(row)(col) == 'O') {
          if(turn == 0) connectState.state(row)(col) = 'R'
          else connectState.state(row)(col) = 'Y'
          empty = true
        }
        else row = row - 1
      }
      (connectState, true)
    }
  }

  def draw(frame: JFrame, board: Array[Array[Char]]) ={
    frame.getContentPane().removeAll()
    frame.getContentPane().setBackground(Color.lightGray)
    frame.setBounds(10,10,512,512);
    var pn = new JPanel(null){
      override def paint(graphics: Graphics): Unit = {
        for (i <- 0 to 5) {
          for (j <- 0 to 6) {
            graphics.setColor(Color.blue)
            graphics.fillRect(j * 64, i * 64, 64, 64)
            if (board(i)(j) == 'R') {
              graphics.setColor(Color.RED)
              graphics.fillOval(j * 64, i * 64, 64, 64)
            }
            else if (board(i)(j) == 'Y') {
              graphics.setColor(Color.YELLOW)
              graphics.fillOval(j * 64, i * 64, 64, 64)
            }
            else {
              graphics.setColor(Color.WHITE)
              graphics.fillOval(j * 64, i * 64, 64, 64)
            }
          }
        }
      }
    }

    /*

    var jt = new JTextField()
    //jt.setBackground(Color.BLUE)
    jt.setBounds(530,10, 150, 35)
    var turn = new JLabel()
    turn.setText("Player1")
    turn.setBounds(530,600, 100,500)
    pn.add(jt)
    pn.add(turn)
     */
    frame.add(pn)
    frame.setDefaultCloseOperation(3)
    frame.setVisible(true)
  }



}
