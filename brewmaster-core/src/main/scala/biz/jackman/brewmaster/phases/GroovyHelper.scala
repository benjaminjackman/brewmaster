package biz.jackman.brewmaster.phases

import groovy.util.Node
import java.util.{List=>JList}
import collection.jcl.Conversions._


object GroovyHelper {
  /** Gets all the child of a groovyNode
   */
  def getSettingsChildren(groovyNode : Node) : List[Node] = {
    groovyNode.children.asInstanceOf[JList[Node]].toList
  }
  
  /** Gets all the child of a node with a certain name
   */
  def getSettingsChildrenNamed(groovyNode : Node, name : String) : List[Node] = {
    getSettingsChildren(groovyNode).filter(_.name == name)
  } 

  /** Gets the child of a groovy settigns node with the given name
   */
  def getSettingsChildSingle(groovyNode : Node, name : String) : Either[String, Option[Node]] = {
    getSettingsChildrenNamed(groovyNode, name) match {
      	case x::y::xs =>
      	  Left("More than 1 container node for a container field")
        case Nil => 
          Right(None)
        case n::ns => 
          Right(Some(n))
      }
  }
  
  /** Gets a child of a groovy settings node 
   */
  def getSettingsChildSingle(groovyNode : Node) : Either[String, Option[Node]] = {
  	getSettingsChildren(groovyNode) match {
    	case x::y::xs =>
    	  Left("Child nodes can only have one instance")
    	case Nil => 
    	  Right(None)
      case n::ns => 
        Right(Some(n))
    }    
  }
}
