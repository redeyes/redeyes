package redeyes.util

trait Strings {
  final def vectorToString(vector: Vector[Char]): String = {
    val builder = new scala.collection.mutable.StringBuilder()
  
    var i = 0
    while (i < vector.length) {
      builder += vector(i)
      i = i + 1
    }
  
    builder.toString
  }
  
  final def listToString(list: List[Char]): String = {
    val builder = new scala.collection.mutable.StringBuilder()
  
    var i = 0
    var cur = list
    while (!cur.isEmpty) {
      builder += cur.head
      cur = cur.tail
      i = i + 1
    }

    builder.toString
  }
}
object Strings extends Strings