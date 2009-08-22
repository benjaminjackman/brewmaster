package biz.jackman.brewmaster.inject

import com.google.inject.Injector

final case class GuiceBrewjector(injector : Injector) extends Brewjector {
	
  override def getInstance[A](clazz : Class[A]) = {
	  try {
	  	Right(injector.getInstance(clazz))
	  } catch {
	    case e => Left(new BrewjectionError {
	      override val throwable = Some(e)
        override val errorMessage = e.toString
	    })
	  }
	}
 
}
