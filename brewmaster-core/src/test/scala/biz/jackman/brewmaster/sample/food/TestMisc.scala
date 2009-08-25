package biz.jackman.brewmaster.sample.food

import com.google.inject.{TypeLiteral, Key, AbstractModule, Provider, Module, Binder, MembersInjector, Injector, Guice, Inject}
import com.google.inject.matcher.Matchers
import com.google.inject.spi.{TypeListener, TypeEncounter}
import java.lang.reflect.{ParameterizedType, Field}
import biz.jackman.brewmaster.annotations.Attribute
import org.scalatest.Suite

class TestMisc extends Suite {
  case class AttributeInjector[T](field : Field) extends MembersInjector[T] {
  	field setAccessible true
    override def injectMembers(t : T) {
      println("Injecting members")
      field.set(t, null)
    }
  }
  
  val brewmasterTypeListener = new TypeListener {
    override def hear[T](typeLiteral : TypeLiteral[T], typeEncounter : TypeEncounter[T]) {
      typeLiteral.getRawType.getDeclaredFields.foreach { field =>
        if (field.isAnnotationPresent(classOf[Attribute])) {
          typeEncounter.register(new AttributeInjector[T](field))
        }
      }
    }
  }
  
  val module = new AbstractModule {
    override def configure() {
      //binder.bindListener(Matchers.any, brewmasterTypeListener)
    }
  }
  
  class Foo (@Attribute val x : Int,
             @Attribute val y : Long,
             @Attribute val z : String) {
    @Inject() def this() = this(0,0,null)
  }
  
//	test("Just a sample") {
//	  val injector = Guice.createInjector(module)
//	  val foo = injector.getInstance(classOf[Foo])
//    foo.x
//	}
}
