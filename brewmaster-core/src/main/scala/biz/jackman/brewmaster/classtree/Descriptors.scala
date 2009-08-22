package biz.jackman.brewmaster.classtree


import java.lang.reflect.Field
import java.lang.reflect.Modifier
import annotations._

sealed trait AnnotationDescriptor

private object null2Opt{
  def apply(value : AnyRef) = if (value == null) None else Some(value)
}

final case class TypeDescriptor(val clazz : Class[_],
                                val infusable : Boolean,
                                val name : String,
                                val inherits : Boolean,
                                val fields : List[FieldDescriptor]) extends AnnotationDescriptor with NamedDescriptor {
  
  //Gets the field that is the id field
  //Warning if two fields are defined as being the id
  //field then the first one found will be used
//  lazy val id : Option[AttributeDescriptor] = fields.find{
//    _ match {
//      case f : AttributeDescriptor => f.id
//      case _ => false
//    }
//  }.map(_.asInstanceOf[AttributeDescriptor])
  
  type NamedContainer = ContainerDescriptor with NamedDescriptor    
  lazy val childMap : Map[String, NamedContainer] = {
    Map() ++ fields.flatMap{ _ match {
    	case field : NamedContainer => List((field.name, field))
    	case _ => Nil
    }}
  }
  
  lazy val attributeMap : Map[String, AttributeDescriptor] = Map() ++ fields.flatMap{ _ match {
    case field : AttributeDescriptor => List((field.name, field))
    case _ => Nil
  }}

  
}

trait FieldDescriptor extends AnnotationDescriptor {
	type FieldType
  val field : Field
  
  field setAccessible true
  
  def get(instance : Any) : FieldType = field.get(instance).asInstanceOf[FieldType]

}

trait SettableField extends FieldDescriptor {

  def set(instance : Any, value : Any) {
	  field.set(instance, value)
	}

}

trait NamedDescriptor extends AnnotationDescriptor {
  val name : String  
}


final case class AttributeDescriptor( val field : Field,
                                      val name : String,
                                      val required : Boolean, 
                                      val immutable : Boolean, 
                                      val defaultValue : Option[String]) extends FieldDescriptor with SettableField with NamedDescriptor {
  override type FieldType = Any
}

final case class UpdaterDescriptor(override val field : Field) extends FieldDescriptor {
  override type FieldType = Updatable
}

trait ContainerDescriptor extends FieldDescriptor {
  val base : TypeDescriptor
  val derived : List[TypeDescriptor]
  lazy val classMap : Map[Class[_], TypeDescriptor]= Map[Class[_], TypeDescriptor]() ++ (base::derived).map(t=>(t.clazz, t))
  lazy val nameMap : Map[String, TypeDescriptor]= Map[String, TypeDescriptor]() ++ (base::derived).map(t=>(t.name, t))
}

final case class ChildDescriptor( override val field : Field,
                                  override val name : String,
                                  override val base : TypeDescriptor,
                                  override val derived : List[TypeDescriptor],
                                  val required : Boolean,
                                  val immutable : Boolean) extends ContainerDescriptor with SettableField with NamedDescriptor  {
  override type FieldType = AnyRef
  def getChildDescriptor(typeInstance : AnyRef) : Option[(AnyRef, TypeDescriptor)]= {
    val childInstance = get(typeInstance)
    classMap.get(childInstance.getClass) match {
      case None => None
      case Some(childTypeDesc) => Some((childInstance, childTypeDesc))
    }
  }
}

abstract class MapDescriptor(override val field : Field,
                    					 override val name : String,
                               val keyType : Class[_],
                               override val base : TypeDescriptor,
                               override val derived : List[TypeDescriptor],
                               val minSize : Int,
                               val immutable : Boolean) extends ContainerDescriptor with MapWrapperProvider with NamedDescriptor { self =>
  override type FieldType = AnyRef                              
}


trait MapWrapperProvider { self : MapDescriptor =>
  type MapWrapperType <: MapWrapper
  def create(typeInstance : AnyRef, entries : Iterable[(AnyRef, AnyRef)]) : MapWrapperType
  def getFromType(typeInstance : AnyRef) : MapWrapperType
}

trait ScalaImmutableMapWrapperProvider extends MapWrapperProvider { self : MapDescriptor =>
  override type MapWrapperType = ScalaImmutableMapWrapper
  override def create(typeInstance : AnyRef, entries : Iterable[(AnyRef, AnyRef)]) : MapWrapperType = {
    val mapInstance = field.get(typeInstance).asInstanceOf[collection.immutable.Map[AnyRef, AnyRef]]
    new ScalaImmutableMapWrapper {
      override val descriptor = self
      override val instance = mapInstance ++ entries
    }
  }
  override def getFromType(typeInstance : AnyRef) : MapWrapperType = {
    val mapInstance = field.get(typeInstance).asInstanceOf[collection.immutable.Map[AnyRef, AnyRef]]
    new ScalaImmutableMapWrapper {
      override val descriptor = self
      override val instance = mapInstance
    }
  }
}

trait ScalaMutableMapWrapperProvider extends MapWrapperProvider { self : MapDescriptor =>
  override type MapWrapperType = ScalaMutableMapWrapper
  override def create(typeInstance : AnyRef, entries : Iterable[(AnyRef, AnyRef)]) : MapWrapperType = {
		val mapInstance = field.get(typeInstance).asInstanceOf[collection.mutable.Map[AnyRef, AnyRef]]
    mapInstance ++= entries
		new ScalaMutableMapWrapper {
			override val descriptor = self
			override val instance = mapInstance 
		}
  }
  override def getFromType(typeInstance : AnyRef) : MapWrapperType = {
		val mapInstance = field.get(typeInstance).asInstanceOf[collection.mutable.Map[AnyRef, AnyRef]]
		new ScalaMutableMapWrapper {
			override val descriptor = self
			override val instance = mapInstance
		}
  }
  
}

trait JavaMapWrapperProvider extends MapWrapperProvider { self : MapDescriptor =>
  override type MapWrapperType = JavaMapWrapper
  override def create(typeInstance : AnyRef, entries : Iterable[(AnyRef, AnyRef)]) : MapWrapperType = {
		val mapInstance = field.get(typeInstance).asInstanceOf[java.util.Map[AnyRef, AnyRef]]
    entries.foreach { case (key, value) =>  mapInstance.put(key, value)}
		new JavaMapWrapper {
			override val descriptor = self
			override val instance = mapInstance
		}
  }
  override def getFromType(typeInstance : AnyRef) : MapWrapperType = {
		val mapInstance = field.get(typeInstance).asInstanceOf[java.util.Map[AnyRef, AnyRef]]
		new JavaMapWrapper {
			override val descriptor = self
			override val instance = mapInstance
		}
  }
}


trait MapWrapper {
  type MapType
  val descriptor : MapDescriptor
  val instance : MapType
  
  private[this] def checkKey[A](key : AnyRef)(body : =>Either[String, A]) : Either[String, A] = {
    val keyClass = key.getClass
    if (descriptor.keyType.isAssignableFrom(keyClass)) {
    	body
    } else {
      Left("Cannot munge key[%s] into the type[%s] this map needs for it's keys" format (keyClass,descriptor.keyType) )
    }
  }
  
  private[this] def checkValue[A](value : AnyRef)(body : =>Either[String, A]) : Either[String, A] = {
  		val valueClass = value.getClass
  		if (descriptor.base.clazz.isAssignableFrom(valueClass)) {
  			body
  		} else {
  			Left("Cannot munge value[%s] into the type[%s] this map needs for it's values" format (valueClass,descriptor.base.clazz) )
  		}
  }
  
  final def get(key : AnyRef) : Either[String, Option[AnyRef]] = checkKey(key)(getInternal(key))
  protected[this] def removeKey(key : AnyRef) : Either[String, Unit] = checkKey(key)(removeKeyInternal(key))
  protected[this] def put(key : AnyRef, value : AnyRef) : Either[String, Unit] = checkKey(key)(checkValue(value)(putInternal(key, value)))
  
  protected[this] def getInternal(key : AnyRef) : Either[String, Option[AnyRef]]
  protected[this] def removeKeyInternal(key : AnyRef) : Either[String, Unit]
  protected[this] def putInternal(key : AnyRef, value : AnyRef) : Either[String, Unit]
  def empty : Boolean
  def size : Int 
  def entries : Iterable[(AnyRef, AnyRef)] 
  def entriesWithDesc : Iterable[(TypeDescriptor, (AnyRef, AnyRef))] = {
    entries.map { case (key, value) =>
      val typeDesc = descriptor.classMap(value.getClass)
      (typeDesc, (key, value))
    }
  }
}


trait ScalaMapWrapper extends MapWrapper {
  type ScalaMapType <: collection.Map[AnyRef, AnyRef]
  override final type MapType = ScalaMapType
  override def getInternal(key : AnyRef) = Right(instance.get(key))
  override def empty = instance.isEmpty
  override def size = instance.size
  override def entries = instance
}

trait ScalaMutableMapWrapper extends ScalaMapWrapper {
  override type ScalaMapType = collection.mutable.Map[AnyRef, AnyRef]
  override def putInternal(key : AnyRef, value : AnyRef) = Right(instance(key) = value)
  override def removeKeyInternal(key : AnyRef) = Right(instance.removeKey(key))
}

trait ScalaImmutableMapWrapper extends ScalaMapWrapper {
	override type ScalaMapType = collection.immutable.Map[AnyRef, AnyRef]
  override def putInternal(key : AnyRef, value : AnyRef) = Left("Cannot add entries to an immutable scala map")
  override def removeKeyInternal(key : AnyRef) = Left("Cannot remove entries from an immutable scala map")
}

trait JavaMapWrapper extends MapWrapper {
  override final type MapType = java.util.Map[AnyRef, AnyRef]
  override def getInternal(key : AnyRef) = Right(null2Opt(instance.get(key)))
  override def removeKeyInternal(key : AnyRef) = Right(instance.remove(key))
  override def putInternal(key : AnyRef, value : AnyRef) = Right(instance.put(key, value))
  override def empty = instance.isEmpty
  override def size = instance.size
  override def entries = {
    import collection.jcl.Conversions._
    instance
  }
}

