package redeyes.util

class IdentityMemo {
  val memoMap = new java.util.IdentityHashMap[AnyRef, AnyRef]

  def apply[A, B](f: A => B): A => B = (a: A) => {
    var result = memoMap.get(a).asInstanceOf[B]

    if (result == null) {
      result = f(a)

      memoMap.put(a.asInstanceOf[AnyRef], result.asInstanceOf[AnyRef])
    }

    result
  }
}