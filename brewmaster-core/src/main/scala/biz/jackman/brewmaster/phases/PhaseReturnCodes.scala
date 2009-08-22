package biz.jackman.brewmaster.phases

import classtree._
import groovy.util.Node

object PhaseErrorCodes {
  sealed trait Result
 
  trait Error extends Result{  
    def errorMessage : String
    def tips : List[String] = Nil
    def exceptionOpt : Option[Throwable] = None
    def toException = {
      val message = errorMessage + (if (tips.isEmpty) "" else tips.mkString("\n"))
      exceptionOpt match {
        case None =>
          new Exception(message)
        case Some(e) =>
          new Exception(message, e)
      }
    }
  }
  sealed trait Success extends Result

  final case class ExceptionError(typeDesc : TypeDescriptor, exception : Throwable) extends Error {
    override lazy val errorMessage = "Type[%s] throw an exception[%s]" format (typeDesc, exception)
    override lazy val exceptionOpt = Some(exception)
  }
  
  final case class MapCannotBeNull(typeDesc : TypeDescriptor, mapDesc : MapDescriptor) extends Error {
    override lazy val errorMessage = "Map[%s] was null in type[%s]" format (mapDesc, typeDesc)
    override lazy val tips = "Be sure to initialize your maps to a concrete type, this concrete type is then added to, this way the Brewmaster know what type of map you want"::Nil 
  }
  
  final case class MapMustStartEmpty(typeDesc : TypeDescriptor, mapDesc : MapDescriptor) extends Error {
  	override lazy val errorMessage = "Map[%s] was not empty in type[%s]" format (mapDesc, typeDesc)
  	override lazy val tips = "Maps must be empty before they are initialized"::Nil 
  }
  
  final case class TypeErrors(typeDesc : TypeDescriptor, errors : List[Error]) extends Error {
    override lazy val errorMessage = "Type[%s] had errors:%s" format(typeDesc, errors.map(_.errorMessage).mkString("\n"))
  }
  
  sealed trait AttributeResult {
    val attrDesc : AttributeDescriptor
  }
  
  sealed trait AttributeInitSuccess extends AttributeResult with Success
  sealed trait AttributeRefreshSuccess extends AttributeResult with Success
  sealed trait AttributeError extends AttributeResult with Error
  
  final case class AttributeMutationError(override val attrDesc : AttributeDescriptor,
  																					val currentValue : Any,
  																					val newValue : Any) extends AttributeError {
    override lazy val errorMessage = {
      "Cannot Change Immutable Attribute: Attribute[%s] CurrentValue[%s] -> NewValue[%s]" format (attrDesc, currentValue, newValue) 
    }
    
    override lazy val tips = {
      "Ensure that an appropriate equals method is defined for the attribute's type."::
        Nil
    }
  }
  
  final case class AttributeTypeError(override val attrDesc : AttributeDescriptor,val newValue : Any) extends AttributeError {
  	override lazy val errorMessage = {
  			"Type[%s] Not Assignable to Attribute: Attribute[%s] NewValue[%s]" format (newValue.asInstanceOf[AnyRef].getClass,  attrDesc, newValue)
  	}
  }
  
  final case class AttributeSettingsError(override val attrDesc : AttributeDescriptor, 
                                          reason : String, 
                                          override val exceptionOpt : Option[Exception]) extends AttributeError {
    override lazy val errorMessage = {
      "Error %s: Attribute[%s]" format attrDesc
    }
  } 
  	
  final case class AttributeSet(override val attrDesc : AttributeDescriptor, 
                                newValue : Any) extends AttributeInitSuccess
  
  final case class AttributeChanged(override val attrDesc : AttributeDescriptor,
  														val currentValue : Any,
  														val newValue : Any) extends AttributeRefreshSuccess
  
  final case class AttributeUnchanged(override val attrDesc : AttributeDescriptor,
  															val currentValue : Any) extends AttributeRefreshSuccess	 
  
  
  
  sealed trait ChildResult {
    val childDesc : ChildDescriptor
  }
  
  sealed trait ChildInitSuccess extends ChildResult with Success
  sealed trait ChildRefreshSuccess extends ChildResult with Success
  sealed trait ChildError extends ChildResult with Error

  /** 
   * Used to indicate that a child should be added
   */
  final case class ChildAdd(childDesc : ChildDescriptor, 
  															childTypeDesc : TypeDescriptor,
  															groovyNode : Node) extends ChildInitSuccess
  
  /** Used to indicate that a child has no settings
   */
  final case class ChildBlank(childDesc : ChildDescriptor) extends ChildInitSuccess with ChildRefreshSuccess
  
  /** 
   * Used to indicate that a child should be removed from the tree
   */
  final case class ChildRemove(childDesc : ChildDescriptor, 
                               currentInstance : AnyRef) extends ChildRefreshSuccess
  
  /** 
   * Used to indicate that an existing child should be replaced
   */
  final case class ChildRefresh(childDesc : ChildDescriptor,
                                currentInstance : AnyRef,
  															childTypeDesc : TypeDescriptor,
  															groovyNode : Node) extends ChildRefreshSuccess
  
  /** 
   * Used to indicate that an exisiting child should be refreshed 
   */
  final case class ChildReplace(childDesc : ChildDescriptor,
                                currentInstance : AnyRef,
  															newChildTypeDesc : TypeDescriptor,
  															groovyNode : Node) extends ChildRefreshSuccess
  
  final case class ChildSettingsNodeError(childDesc : ChildDescriptor, 
                                          reason : String) extends ChildError {
    override lazy val errorMessage = "Child[%s] had an error with its settings [%s]" format (childDesc, reason)
    
  }
  
  final case class ChildRequiredError(override val childDesc : ChildDescriptor) extends ChildError {
  	  override lazy val errorMessage = {
  	    "Missing required child[%s]" format childDesc
  	  }
  }
  
  final case class ChildNameNotFoundError(override val childDesc : ChildDescriptor, name : String) extends ChildError {
    override lazy val errorMessage = {
      "Child[%s] was unable to find the name[%s] specified in the settings node as being a valid name for a base or derived child type" format (childDesc, name)
    }
  }
  
  final case class ChildMutatedError(override val childDesc : ChildDescriptor) extends ChildError {
  	  override lazy val errorMessage = {
  	    "Attempted to modify an immutable child[%s], (probably tried to change it's type)" format childDesc
  	  }
  }

  sealed trait MapResult {
    val mapDesc : MapDescriptor
  }

  
  sealed trait MapInitSuccess extends MapResult with Success
  sealed trait MapRefreshSuccess extends MapResult with Success
  sealed trait MapError extends MapResult with Error
  
  
  final case class MapBlank(mapDesc : MapDescriptor) extends MapInitSuccess
  final case class MapAdd(mapDesc : MapDescriptor, 
                          mapEntries : Map[AnyRef, MapEntryAdd]) extends MapInitSuccess
  
  final case class MapUpdate(mapDesc : MapDescriptor,
  													 mapEntryActions : Map[AnyRef, MapEntryAction]) extends MapRefreshSuccess
  
  sealed trait MapEntryAction {
    val mapEntryTypeDesc : TypeDescriptor
    val key : AnyRef
  }
  
  
  final case class MapEntryAdd(mapEntryTypeDesc : TypeDescriptor,
                               key : AnyRef,
  														 mapEntryGNode : Node) extends MapEntryAction
  
  final case class MapEntryRefresh(mapEntryTypeDesc : TypeDescriptor,
                               key : AnyRef,
  														 mapEntryGNode : Node,
  														 oldInstance : AnyRef) extends MapEntryAction
  
  final case class MapEntryReplace(mapEntryTypeDesc : TypeDescriptor,
  																 key : AnyRef,
  																 mapEntryGNode : Node,
  																 oldInstance : AnyRef) extends MapEntryAction
  
  final case class MapEntryRemove(mapEntryTypeDesc : TypeDescriptor,
                               key : AnyRef,
  														oldInstance : AnyRef) extends MapEntryAction
  
  final case class MapSettingsNodeError(mapDesc : MapDescriptor, 
                                          reason : String) extends MapError {
    override lazy val errorMessage = "Map[%s] had an error with its settings [%s]" format (mapDesc, reason)
    
  }

  final case class MapTooSmallError(mapDesc : MapDescriptor, size : Int, minSize : Int) extends MapError {
    override lazy val errorMessage = "Map[%s] entries count[%d] below minSize[%d]" format (mapDesc, size, minSize)
  }
  
  final case class MapEntryNameNotFoundError(override val mapDesc : MapDescriptor, name : String) extends MapError {
    override lazy val errorMessage = {
      "Map[%s] was unable to find the name[%s] specified in the settings node as being a valid name for a base or derived child type" format (mapDesc, name)
    }
  }
  
  final case class MapEntryMissingKeyError(override val mapDesc : MapDescriptor, val mapEntryTypeDesc : TypeDescriptor) extends MapError {
  	override lazy val errorMessage = {
  			"Map[%s] had an entry[%s] without a $key attribute specified" format (mapDesc, mapEntryTypeDesc)
  	}
  }
  
  final case class MapEntryTypeNotFoundError(override val mapDesc : MapDescriptor, val entryType : Class[_]) extends MapError {
    override lazy val errorMessage = {
      "Map[%s] descriptor did not contain a descriptor for class[%s]" format (mapDesc, entryType)
    }
  }
  
  final case class MapEntriesError(override val mapDesc : MapDescriptor, mapEntries : Iterable[Either[MapError, MapEntryAction]]) extends MapError {
    override lazy val errorMessage = {
      "Map[%s] had the following errors with its entries Error[%s]" format(mapDesc, 
         mapEntries.flatMap(e=>e.left.toOption.map(_.errorMessage)).mkString)
    }
  }
  
  final case class MapMutatedError(override val mapDesc : MapDescriptor) extends MapError {
    override lazy val errorMessage = "Map[%s] Attempted to modify immutable map" format mapDesc 
  }
  
}