package biz.jackman.brewmaster.instancetree

import classtree._
import groovy.util.Node
import collection.mutable.ListBuffer

/** The first part of the file are the builder classes. They are used to
 * instantiate the object below
 * 
 */

trait RootNodeBuilder { self =>
  val groovyNode : Node
  val directorDescriptors : List[TypeDescriptor]
  
  private[this] val children = new ListBuffer[TypeInstanceNodeBuilder]
  final def addChild(child : TypeInstanceNodeBuilder) {
    children append child
  }
  final def build : RootNode = {
    var node : RootNode = null
    node = new RootNode {
      override val directorDescriptors = self.directorDescriptors
      override val groovyNode = self.groovyNode
      override val children = self.children.toList.map{ tinb => 
      	tinb.build(this)
      }
    }
    node
  }
}

sealed trait Phase
case object InitializePhase extends Phase
case object RefreshPhase extends Phase

trait TypeInstanceNodeBuilder { self =>
  private[this] val children = new ListBuffer[FieldNodeBuilder]
  val instance : Any
  val descriptor : TypeDescriptor 
  val groovyNode : Node
  val phase : Phase
  
  final def addChild(child : FieldNodeBuilder) {
    children append child
  }
  
  final def build(parentContainer : ContainerNode) : TypeInstanceNode = {
    new TypeInstanceNode {
      override val phase = self.phase
      override val parent = parentContainer
      override val instance = self.instance
      override val descriptor = self.descriptor
      override val groovyNode = self.groovyNode
      override val children : Map[FieldDescriptor, FieldNode] = Map(self.children.toList.map { childBuilder =>
        (childBuilder.descriptor, childBuilder.build(this))
      }:_*)
    }
  }
}

sealed trait FieldNodeBuilder {
  type FieldNodeType <: FieldNode
  type FieldNodeDescriptorType <: FieldDescriptor
  val descriptor : FieldNodeDescriptorType
  def build(parentContainer : ContainerNode) : FieldNodeType
}

final case class FieldInstanceNodeBuilder(val instance : Any, 
                                          override val descriptor : AttributeDescriptor) extends FieldNodeBuilder { self =>
  final override type FieldNodeType = FieldInstanceNode
  final override type FieldNodeDescriptorType = AttributeDescriptor

  final override def build(parentContainer : ContainerNode) : FieldNodeType = {
    new FieldInstanceNode {
      override val parent = parentContainer
      override val instance = self.instance
      override val descriptor = self.descriptor
    }
  }
}

trait MapInstanceNodeBuilder extends FieldNodeBuilder { self =>
  final override type FieldNodeType = MapInstanceNode
  final override type FieldNodeDescriptorType = MapDescriptor
  val instance : Any
  val groovyNode : Node
  
  private[this] val children = new ListBuffer[TypeInstanceNodeBuilder]
  final def addChild(child : TypeInstanceNodeBuilder) {
    children append child
  }
  
  final override def build(parentContainer : ContainerNode) : FieldNodeType = {
    new MapInstanceNode {
      override val parent = parentContainer
      override val instance = self.instance
      override val descriptor = self.descriptor
      override val groovyNode = self.groovyNode
      override val children = self.children.toList.map{ child =>
        child.build(this)
      }
    }
  }
}

trait ChildInstanceNodeBuilder extends FieldNodeBuilder { self =>
  final override type FieldNodeType = ChildInstanceNode
  final override type FieldNodeDescriptorType = ChildDescriptor
  val instance : Any
  val groovyNode : Node
  var child : TypeInstanceNodeBuilder = _
  
  final override def build(parentContainer : ContainerNode) : FieldNodeType = {
    new ChildInstanceNode {
      override val parent = parentContainer
      override val instance = self.instance
      override val descriptor = self.descriptor
      override val groovyNode = self.groovyNode
      override val child = self.child.build(this)
    }
  }
}



final case class UpdaterInstanceNodeBuilder(val instance : annotations.Updatable, val descriptor : UpdaterDescriptor) extends FieldNodeBuilder { self =>
  final override type FieldNodeType = UpdaterInstanceNode
  final override type FieldNodeDescriptorType = UpdaterDescriptor  
  
  final override def build(parentContainer : ContainerNode) : FieldNodeType = {
    new UpdaterInstanceNode {
      override val parent = parentContainer
      override val instance = self.instance
      override val descriptor = self.descriptor
    }
  }
}

/** Trait for nodes that have 
 * an instance bound to themselves
 */
sealed trait InstanceNode {
  type DescriptorType <: AnnotationDescriptor
  type InstanceType <: Any
  val instance : InstanceType
  val parent : ContainerNode
  val descriptor : DescriptorType
}

/** Marker Trait for nodes that are Fields
 */
sealed trait FieldNode

/** Marker Trait for nodes that can contain other nodes
 */
sealed trait ContainerNode

/** This node is special as it has no instance, it only points to all the 
 * sub nodes that actually do
 */
trait RootNode extends ContainerNode with GroovyNode {
  val children : List[TypeInstanceNode]
  val directorDescriptors : List[TypeDescriptor]
}

trait GroovyNode {
  val groovyNode : Node
}

/** The nodes contains fields, maps, updaters, and children
 */
trait TypeInstanceNode extends InstanceNode with ContainerNode with GroovyNode {
  override type DescriptorType = TypeDescriptor
  override type InstanceType = Any
  val phase : Phase
  val children : Map[FieldDescriptor, FieldNode]
}

/** Fields Instances, these are the leaves of the tree
 */
trait FieldInstanceNode extends InstanceNode with FieldNode {
	override type DescriptorType = FieldDescriptor
	override type InstanceType = Any
}

/** Map Containers, these nodes will have children that have a key 
 * (indicated by their @Id field)
 */
trait MapInstanceNode extends InstanceNode with FieldNode with ContainerNode with GroovyNode {
	override type DescriptorType = MapDescriptor
	override type InstanceType = Any
  val children : List[TypeInstanceNode] 
}

/** Child containers, these nodes will have one and only one child
 */
trait ChildInstanceNode extends InstanceNode with FieldNode with ContainerNode with GroovyNode {
	override type DescriptorType = ChildDescriptor
  override type InstanceType = Any
  val child : TypeInstanceNode 
}

/** Updater instances, used to communicate various update messages to
 * the instances in the tree
 */
trait UpdaterInstanceNode extends InstanceNode with FieldNode  {
  override type DescriptorType = UpdaterDescriptor
  override type InstanceType = annotations.Updatable
}