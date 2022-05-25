package States

class ConnectFourState {
  var state: Array[Array[Char]] = Array.ofDim[Char](6, 7)
  state = state.map(row => row.map(x => 'O'))
}
