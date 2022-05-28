import javax.swing.JFrame

/**
 *
 * @tparam S generic type for the state class
 * @tparam I generic type for the input class
 */
trait Game[S, I] {
  def drawer(frame:JFrame, state: S): Unit;
  def controller(state: S, input: I, turn: Int): (S, Boolean);

}
