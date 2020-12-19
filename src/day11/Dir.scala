package day11

sealed trait Dir {
  def right(): Dir

  def left(): Dir

  def asCoords(): (Int, Int)
}

case object Up extends Dir {
  override def right(): Dir = Right

  override def left(): Dir = Left

  override def asCoords(): (Int, Int) = (0, 1)
}

case object Down extends Dir {
  override def right(): Dir = Left

  override def left(): Dir = Right

  override def asCoords(): (Int, Int) = (0, -1)
}

case object Left extends Dir {
  override def right(): Dir = Up

  override def left(): Dir = Down

  override def asCoords(): (Int, Int) = (-1, 0)
}

case object Right extends Dir {
  override def right(): Dir = Down

  override def left(): Dir = Up

  override def asCoords(): (Int, Int) = (1, 0)
}