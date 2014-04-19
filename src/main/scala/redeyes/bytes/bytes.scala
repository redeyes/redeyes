package redeyes.bytes

case class ByteArray(private val value: Array[Byte]) {
  def unsafeAsArray = value

  def toArray = value.clone.asInstanceOf[Array[Byte]]
}