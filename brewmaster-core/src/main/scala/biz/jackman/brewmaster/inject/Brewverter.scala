package biz.jackman.brewmaster.inject

trait Brewverter {
	def convert[A,B](instance : A, clazz : Class[B]) : Either[BrewjectionError, B]
}
