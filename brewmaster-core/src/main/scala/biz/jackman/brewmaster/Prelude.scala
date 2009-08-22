package biz.jackman.brewmaster

/** Predef for brewmaster, maybe package objects will obsolete this
 */
private[brewmaster] object Prelude {
  implicit def any2GetAnyClass(x : Any) = new GetAnyClass(x) 
	class GetAnyClass(val x : Any) {
	  def getAnyClass = x.asInstanceOf[AnyRef].getClass
  }
}
