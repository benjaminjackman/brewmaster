package biz.jackman.brewmaster.classtree

import annotations._
import java.lang.reflect.{Field, Modifier}
import java.lang.annotation.Annotation


object ClassTreeGenerator {
	def getAnnotation[A <: Annotation](field : Field, ann : Class[A]) : Option[A] = {
	  if (field isAnnotationPresent ann) {
	    Some(field getAnnotation ann )
	  } else {
	    None
	  }
	}
 
 	def getAnnotation[A <: Annotation](clazz : Class[_], ann : Class[A]) : Option[A] = {
	  if (clazz isAnnotationPresent ann) {
	    Some(clazz getAnnotation ann )
	  } else {
	    None
	  }
	}


	def getFieldDescriptors(clazz : Class[_]) : List[_ <: FieldDescriptor] = {
	  val fields = clazz.getDeclaredFields
	  fields.toList.flatMap{field : Field =>
	    def name = getAnnotation(field, classOf[Tag]) match {
	      	case None => field.getName
	      	case Some(ann) => ann.value match {
	      		case "" => error("Field[%s] Empty tag name in annotation" format field)
	      		case s => s  
	      	} 
	  	}
     
	    if (field.isAnnotationPresent(classOf[Child])) {
	    	//Get the types that have to be scanned
	    	val types = field.getType :: getAnnotation(field, classOf[ChildTypes]).toList.flatMap {
	    	  _.value.toList}
	    	val base::derived = types.map(getTypeDescriptor(_))
	    	val ann = field.getAnnotation(classOf[Child])
	    	val required = !field.isAnnotationPresent(classOf[Optional])
	    	val immutable = !field.isAnnotationPresent(classOf[Mutable])
      
	    	List(ChildDescriptor(field, name, base, derived, required, immutable))
	    } else if (field.isAnnotationPresent(classOf[Mapped])) {
	      //Ensure the it is of map type
	      //scan the base & derived classes
	      //create the descriptor
	      val ann = field.getAnnotation(classOf[Mapped])
       
	      val keyType = ann.key
	      val minSize = 0
	      val immutable = !field.isAnnotationPresent(classOf[Mutable])
       
       	  val types = ann.base :: ann.derived.toList
	      val base::derived = types.map(getTypeDescriptor(_))
	      
	      val descriptor = field.getType match {
	        case x if (classOf[java.util.Map[_,_]].isAssignableFrom(x)) =>
	          new MapDescriptor(field, name, keyType, base, derived, minSize, immutable) with JavaMapWrapperProvider
	        case x if (classOf[Map[_,_]].isAssignableFrom(x)) =>
	          new MapDescriptor(field, name, keyType, base, derived, minSize, true) with ScalaImmutableMapWrapperProvider
	        case x if (classOf[scala.collection.mutable.Map[_,_]].isAssignableFrom(x)) =>
	          new MapDescriptor(field, name, keyType, base, derived, minSize, immutable) with ScalaMutableMapWrapperProvider
	        case _ =>
	          error("Field[%s] Only Map types can have @Mapped annotation" format (field))
	      }
	   
	      List(descriptor)
       
	    } else if (field.isAnnotationPresent(classOf[Updater])) {
	    	if (classOf[Updatable] isAssignableFrom field.getType) {
	    	  List(UpdaterDescriptor(field))
	    	} else {
	    	  error("Field[%s] type must derive from Updatable trait when @Updater annotation is present" format(field))
	    	}

	    } else if (field.isAnnotationPresent(classOf[Attribute])) {
	      
	      val required = !field.isAnnotationPresent(classOf[Optional])
        val immutable = !field.isAnnotationPresent(classOf[Mutable])

	      val defaultValue = getAnnotation(field, classOf[DefaultValue]) map {_.value}
       
	      List(AttributeDescriptor(field, name, required, immutable, defaultValue))
       
	    } else {
	      Nil
       
	    }
	  }
	}
	
	def getSuperFieldDescriptors(clazz : Class[_]) : List[_ <: FieldDescriptor] = {
	  if (clazz.isAnnotationPresent(classOf[InheritAttributes])) {
	    val superclass = clazz.getSuperclass
	    getSuperFieldDescriptors(superclass):::getFieldDescriptors(superclass)
	  } else {
	    Nil
	  }
	}
  
	def getTypeDescriptor(clazz : Class[_]) : TypeDescriptor = {
	  val name = getAnnotation(clazz, classOf[Tag]) match {
      	case None => clazz.getSimpleName
      	case Some(ann) => ann.value match {
      		case "" => error("Type[%s] Empty tag name in annotation" format(clazz))
      		case s => s  
      	}
	  }
	  
	  val infusable = classOf[Infusable] isAssignableFrom clazz
	  
	  val inherits = clazz.isAnnotationPresent(classOf[InheritAttributes])
	 
	  val fields = getSuperFieldDescriptors(clazz):::getFieldDescriptors(clazz)
   
	  TypeDescriptor(clazz, infusable, name, inherits, fields)
	  
	}
  
}
