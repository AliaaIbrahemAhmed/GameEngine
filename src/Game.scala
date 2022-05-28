/**
 *
 * @tparam S generic type for the state class
 * @tparam I generic type for the input class
 */
trait Game[S, I] {
  def drawer(state: S): Unit;
  def controller(state: S, input: I, turn: Int): (S, Boolean);

}
