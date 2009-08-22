package biz.jackman.brewmaster.inject

/** This is returned as a Left when an injection fails
 */
trait BrewjectionError {
  val throwable : Option[Throwable] = None
  val errorMessage : String
}

/** A trait that does injections
 */
trait Brewjector {
	def getInstance[A <: AnyRef](clazz : Class[A]) : Either[BrewjectionError, A]
  
}