package biz.jackman.brewmaster.annotations


trait UpdatableError {
  val errorMsg : String
  val throwable : Option[Throwable] = None 
}


object Updatable {

}

/** The updatable trait should be used by a field in a class
 * to obtain notifications about when a class is created and when
 * it is safe for that class to start threads. It has will tell a class
 * when it has been removed from the tree thereby informing it of
 * times when it needs to perform cleanup tasks such as killing threads.
 */
trait Updatable {
  implicit def throwable2Error(t : Throwable) = {
    new UpdatableError {
      override val errorMsg = t.toString 
      override val throwable = Some(t)
    } 
  }  
  implicit def throwableOpt2Error(t : Option[Throwable]) = t.map { t=>
      new UpdatableError {
        override val errorMsg = t.toString 
        override val throwable = Some(t)
      }
  }  
  implicit def either2Error(t : Either[Throwable, Unit]) = t match { 
    case Left(t) => 
      Some(new UpdatableError {
        override val errorMsg = t.toString 
        override val throwable = Some(t)
      })
    case Right(_) => None
  }    
	def handle(update : UpdateType) : Option[UpdatableError]
}
