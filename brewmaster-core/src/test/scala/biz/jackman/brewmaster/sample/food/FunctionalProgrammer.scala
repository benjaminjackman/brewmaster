package biz.jackman.brewmaster.sample.food

import biz.jackman.brewmaster.annotations.{InheritAttributes, DefaultValue, Updater, Updatable, UpdateType, UpdateTypes, Attribute}
import com.google.inject.Inject

@InheritAttributes
class FunctionalProgrammer (@Attribute @DefaultValue("\"Scala\"") val favoriteLanguage : String,
                            @Attribute @DefaultValue("2") var strength : Int) extends AbstractEmployee {
  @Inject def this() = this("Scala", 2)

  var startCount = 0

  @Updater
  val u : Updatable = new Updatable {
    override def handle(update : UpdateType) = {
      update match {
        case start : UpdateTypes.Start => 
          startCount += 1
          None
        case _ =>
          None
      }
    }
  }

}
