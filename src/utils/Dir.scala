package utils

sealed trait Dir {
  def right(): Dir

  def left(): Dir

  def toCoords: (Int, Int)

  def fromNumber(i: Int): Dir = i match {
    case 1 => Up
    case 2 => Down
    case 3 => Right
    case 4 => Left
  }

  def toInt: Int
}

case object Dir extends Dir {
  override def right(): Dir = throw new NotImplementedError()

  override def left(): Dir = throw new NotImplementedError()

  override def toCoords: (Int, Int) = throw new NotImplementedError()

  override def toInt: Int = throw new NotImplementedError()
}

case object Up extends Dir {
  override def right(): Dir = Right

  override def left(): Dir = Left

  override def toCoords: (Int, Int) = (0, 1)

  override def toInt: Int = 1
}

case object Down extends Dir {
  override def right(): Dir = Left

  override def left(): Dir = Right

  override def toCoords: (Int, Int) = (0, -1)

  override def toInt: Int = 2
}

case object Right extends Dir {
  override def right(): Dir = Down

  override def left(): Dir = Up

  override def toCoords: (Int, Int) = (1, 0)

  override def toInt: Int = 3
}

case object Left extends Dir {
  override def right(): Dir = Up

  override def left(): Dir = Down

  override def toCoords: (Int, Int) = (-1, 0)

  override def toInt: Int = 4
}
