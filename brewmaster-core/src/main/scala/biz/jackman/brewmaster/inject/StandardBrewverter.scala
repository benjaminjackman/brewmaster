package biz.jackman.brewmaster.inject

import Prelude._
import reflect.Manifest

/** A simple key used to quicken lookups for static converters
 */
private[inject] case class BrewverterKey[A,B](from : Class[A], to : Class[B])

/** The base trait of both static and dynamic brewverters
 * for static from is the class convertered from (and not any super or child class!)
 * for static to is the class convertered to (and not any super or child class!)
 * for dynamic it is more flexible, and these types are allowed to vary and just
 * serve as providing an level of type safety to clients on their side when defining
 * the methods / predicates involved. For examples of how to use these
 * Look at the StandardBrewverter object
 */
private[inject] sealed trait TypedBrewverter[A,B] {
  val from : Class[A]
  val to : Class[B]
}

/** A static brewverter does simple conversions from one type to another, it's from type is placed into a map
 * to speed look up times, however, subclasses of from or to are not handled by static brewverters
 */
object StaticBrewverter {
  /** Creates a brewverter instance
   */
  def apply[A,B](from : Class[A], to : Class[B])(convert : (A)=>B) = {
    new StaticBrewverter[A,B](from, to, convert)
  }
}

final class StaticBrewverter[A,B](val from : Class[A], val to : Class[B], val convert : (A)=>B) extends TypedBrewverter[A,B]{
  val key = BrewverterKey(from, to)
}

/** A dynamic brewverter is able to check to see if it can perform a conversion from a class to another class
 * based on the instance types of the classes required. This allows for things like enums to be converted,
 * where are broad range of types could be handled however the desired concrete output classtype is not known until
 * after the brewverter is actually called.
 */
object DynamicBrewverter {
  def apply[A,B](from : Class[A], to : Class[B])(predicate : (Class[A],Class[B])=>Boolean)(convert : (A,Class[B])=>B) = {
    new DynamicBrewverter(from, to, predicate, convert)
  }
}

final class DynamicBrewverter[A,B](val from : Class[A], val to : Class[B], val predicate : (Class[A],Class[B]) => Boolean, val convert : (A,Class[B])=>B) extends TypedBrewverter[A,B]

/** A Chained brewverter links up with other brewverters to create a greate composite brewverter.
 * Two Chained brewverters can be combined with the ++ method yielding a new ChainedBrewverter that is the
 * combination of the converters in both.
 */
sealed class ChainedBrewverter(private val statics : List[StaticBrewverter[Any,Any]], private val dynamics : List[DynamicBrewverter[Any,Any]]) extends Brewverter {
  lazy val staticMap : Map[BrewverterKey[Any,Any], StaticBrewverter[Any,Any]] = Map() ++ statics.map(c=>(c.key.asInstanceOf[BrewverterKey[Any,Any]], c.asInstanceOf[StaticBrewverter[Any,Any]]))
  //TODO LRU cache for dynamics, with None for not founds
  
  override def convert[A,B](instance : A, clazz : Class[B])  = {
    val instanceClass = instance.getAnyClass.asInstanceOf[Class[Any]]
    val toAnyClass = clazz.asInstanceOf[Class[Any]]
    if (toAnyClass.isAssignableFrom(instanceClass)) {
      Right(instance.asInstanceOf[B])
    } else {
	    staticMap.get(BrewverterKey[Any,Any](instanceClass, toAnyClass)).map(_.convert(instance)).orElse {
	      //TODO LRU CHECK
	      dynamics.find{converter =>
	        converter.predicate(instanceClass, toAnyClass)
	      }.map{converter =>
	        converter.convert(instanceClass, toAnyClass)
	      }
	    }.toRight(new BrewjectionError{
	      override val errorMessage = "Unable to find a converter from[%s] to[%s]" format(instanceClass, toAnyClass)
	    }).asInstanceOf[Either[BrewjectionError, B]]
    }
  }
  
  def ++(other : ChainedBrewverter) : ChainedBrewverter = {
    new ChainedBrewverter(statics:::other.statics, dynamics:::other.dynamics)
  }
}

/** These are the standard brewverters used to convert common datatypes
 */
object StandardBrewverters {
  private[this] val staticLB = new collection.mutable.ListBuffer[StaticBrewverter[_,_]]()
  private[this] val dynLB = new collection.mutable.ListBuffer[DynamicBrewverter[_,_]]()
  
  lazy val staticConverters = staticLB.toList.asInstanceOf[List[StaticBrewverter[Any,Any]]]
  lazy val dynamicConverters = dynLB.toList.asInstanceOf[List[DynamicBrewverter[Any,Any]]]

  private[this] def add(xs : =>List[StaticBrewverter[_,_]]) {
    staticLB ++= xs
  }
  
  private[this] def addD(xs : => List[DynamicBrewverter[_,_]]) {
    dynLB ++= xs
  }
  
  //Boolean
  add {
	  val j = classOf[java.lang.Boolean]
	  val p = java.lang.Boolean.TYPE 
	  val s = classOf[Boolean]
	   List (
	      StaticBrewverter(j, p)(x=>x),
	      StaticBrewverter(p, j)(x=>x),
	      StaticBrewverter(j, s)(x=>x.booleanValue),
	      StaticBrewverter(s, j)(x=>x),
	      StaticBrewverter(p, s)(x=>x.booleanValue),
	      StaticBrewverter(s, p)(x=>x)
	  )
  }
  
  //Byte
  add {
	  val j = classOf[java.lang.Byte]
	  val p = java.lang.Byte.TYPE 
	  val s = classOf[Byte]
	   List (
	      StaticBrewverter(j, p)(x=>x),
	      StaticBrewverter(p, j)(x=>x),
	      StaticBrewverter(j, s)(x=>x.byteValue),
	      StaticBrewverter(s, j)(x=>x),
	      StaticBrewverter(p, s)(x=>x.byteValue),
	      StaticBrewverter(s, p)(x=>x)
	  )
  }

    
  //Short
  add {
	  val j = classOf[java.lang.Short]
	  val p = java.lang.Short.TYPE 
	  val s = classOf[Short]
	   List (
	      StaticBrewverter(j, p)(x=>x),
	      StaticBrewverter(p, j)(x=>x),
	      StaticBrewverter(j, s)(x=>x.shortValue),
	      StaticBrewverter(s, j)(x=>x),
	      StaticBrewverter(p, s)(x=>x.shortValue),
	      StaticBrewverter(s, p)(x=>x)
	  )
  }


  //Integer
  add {
	  val JInt = classOf[java.lang.Integer]
	  val PInt = java.lang.Integer.TYPE 
	  val SInt = classOf[Int]
	  List (
      StaticBrewverter(JInt, PInt)(x=>x),
      StaticBrewverter(PInt, JInt)(x=>x),
      StaticBrewverter(JInt, SInt)(x=>x.intValue),
      StaticBrewverter(SInt, JInt)(x=>x),
      StaticBrewverter(PInt, SInt)(x=>x.intValue),
      StaticBrewverter(SInt, PInt)(x=>x)
	  )
  }
  
  //Float
  add {
	  val j = classOf[java.lang.Float]
	  val p = java.lang.Float.TYPE 
	  val s = classOf[Float]
	  List (
      StaticBrewverter(j, p)(x=>x),
      StaticBrewverter(p, j)(x=>x),
      StaticBrewverter(j, s)(x=>x.floatValue),
      StaticBrewverter(s, j)(x=>x),
      StaticBrewverter(p, s)(x=>x.floatValue),
      StaticBrewverter(s, p)(x=>x)
	  )
  }

  
  //Double
  add {
	  val JDbl = classOf[java.lang.Double]
	  val PDbl = java.lang.Double.TYPE 
	  val SDbl = classOf[Double]
	  List (
      StaticBrewverter(JDbl, PDbl)(x=>x),
      StaticBrewverter(PDbl, JDbl)(x=>x),
      StaticBrewverter(JDbl, SDbl)(x=>x.doubleValue),
      StaticBrewverter(SDbl, JDbl)(x=>x),
      StaticBrewverter(PDbl, SDbl)(x=>x.doubleValue),
      StaticBrewverter(SDbl, PDbl)(x=>x)
	  )
  }
  
  //Char
  add {
	  val j = classOf[java.lang.Character]
	  val p = java.lang.Character.TYPE 
	  val s = classOf[Char]
	  List (
      StaticBrewverter(j, p)(x=>x),
      StaticBrewverter(p, j)(x=>x),
      StaticBrewverter(j, s)(x=>x.charValue),
      StaticBrewverter(s, j)(x=>x),
      StaticBrewverter(p, s)(x=>x.charValue),
      StaticBrewverter(s, p)(x=>x)
	  )
  }

  
  //String, GStrings can mess us up
  add {
	  val j = classOf[java.lang.String]
	  val g = classOf[groovy.lang.GString] 
	  List (
      StaticBrewverter(g, j)(x=>x.toString)
	  )
  }
  
  //Java Enum, 
  //probably did not even need this shit
  //WTF whatever
  addD {
    val e = classOf[java.lang.Enum[_]]
    val g = classOf[groovy.lang.GString]
    val s = classOf[java.lang.String]
    List (
      DynamicBrewverter(s,e){case (f,t) => 
        e.isAssignableFrom(t)
      }{ (i,c) =>
        def valueOf[T <: java.lang.Enum[T]]() = {
          java.lang.Enum.valueOf[T](c.asInstanceOf[Class[T]],i)
        }
        valueOf()
      },
      DynamicBrewverter(g,e){case (f,t) => 
      e.isAssignableFrom(t)
      }{ (i,c) =>
	      def valueOf[T <: java.lang.Enum[T]]() = {
	      		java.lang.Enum.valueOf[T](c.asInstanceOf[Class[T]],i.toString)
	      }
	      valueOf()
      }
    )
  }

}

final case class StandardBrewverter() extends ChainedBrewverter(StandardBrewverters.staticConverters, StandardBrewverters.dynamicConverters)
