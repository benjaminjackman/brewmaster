package biz.jackman.brewmaster.phases


import inject.{Brewjector, Brewverter, BrewjectionError}
import classtree._
import annotations.{UpdateTypes, Infusable}
import groovy.lang.GroovyShell
import groovy.util.Node
import java.util.{List=>JList}
import collection.jcl.Conversions._


object Brewsetter {
  import PhaseErrorCodes._
  import GroovyHelper._
  
  implicit def brewjectionError2PhaseError(err : BrewjectionError) = new Error {
    override lazy val errorMessage = "Brewjection Error " + err.errorMessage
    override lazy val exceptionOpt = err.throwable
  }
  
	private trait Infuser {
	  def apply(blk :  => Either[Error, AnyRef]) : Either[Error, AnyRef]
	}
	
	//This is used for types which do not require infusing
	private object DefaultInfuser extends Infuser {
	  override def apply(blk :  => Either[Error, AnyRef]) = {
	    blk
	  }
	}
	
	//This is used for types which require infusing
	private case class TypeInfuser(infusable : Infusable) extends Infuser {
	  override def apply(blk :  => Either[Error, AnyRef]) = {
	      infusable.infuse(new SettingsAction {
	        override type ReturnType = Either[Error, AnyRef]
	        override def perform : Either[Error, AnyRef] = {
	          blk
	        }
	      })
	  }
	}
 
  private def getInfuser(typeDesc : TypeDescriptor, instance : AnyRef) = {
  	if (typeDesc.infusable) {
      instance match {
        case infusable : Infusable => TypeInfuser(infusable)
      }
    } else {
      DefaultInfuser
    }    
  }
  
  /** Called to make an instance of a type
   */
  def initializeType(typeDesc : TypeDescriptor,
                     instance : AnyRef,
                     typeSettingsNode : Node)(implicit shell : GroovyShell, 
                                              injector : Brewjector,
                     													converter : Brewverter) : Either[Error, AnyRef] = {
    
    //Now we need to get a runner that is going to
    //call the rest of this method, this allows
    //for the use of infusers, so that this
    //type can be initialized on a different thread
    //or behind a lock
    val infuser = getInfuser(typeDesc, instance)
    
    infuser { 
	    val results = typeDesc.fields.flatMap[Error] { fieldDesc =>
	      fieldDesc match {
	        case attrDesc : AttributeDescriptor =>
	          initializeAttributeNode(attrDesc, typeSettingsNode) match {
	            case Left(error) =>
	              List(error)
	            case Right(attrRes) =>
	              try {
	              	attrRes.attrDesc.set(instance, attrRes.newValue)
	              	Nil
	              } catch {
	                case e =>
	                  List(ExceptionError(typeDesc, e))
	              }
	          }
	        case updaterDesc : UpdaterDescriptor =>
	          try {
	          	updaterDesc.get(instance).handle(UpdateTypes.Initialize())
	          	Nil
	          } catch {
	            case e=>
	              List(ExceptionError(typeDesc, e))
	          }
	        case childDesc : ChildDescriptor =>
	          initializeChildNode(childDesc, typeSettingsNode) match {
	            case Left(error) =>
	              List(error)
	            case Right(childResult) =>
	              childResult match {
	                case childAdd : ChildAdd =>
	                  val eChildInstance = childAdd.childDesc.get(instance) match {
	                    case null =>
	                      injector.getInstance(childAdd.childTypeDesc.clazz.asInstanceOf[Class[AnyRef]])
	                    case childInstance =>
	                      //We may want to force children to
	                      //be null, or perhaps have an default value
                        //system
	                      Right(childInstance)
	                  } 
	                  eChildInstance match {
	                    case Left(error) => List(error)
	                    case Right(childInstance) =>
	                      //Recursive!
	                      childDesc.set(instance, childInstance)
	                      initializeType(childAdd.childTypeDesc, childInstance, childAdd.groovyNode) match {
	                        case Left(error) => List(error)
	                        case Right(_) => Nil
	                      }
	                  }
	                case childBlank : ChildBlank =>
	                  Nil
	                  //Do nothing, may be bad news if there is already an instance
	                  //Oh well!
	              }
	          }
	        case mapDesc : MapDescriptor =>
	          //Instantiate as needed
	          initializeMapNode(mapDesc, typeSettingsNode) match {
	            case Left(e) => List(e)
	            case Right(mapResult) =>
	              mapResult match {
	                case mapAdd : MapAdd =>
	                  
	                  //Create all of the entries that need to be added
	                  val entryResults = mapAdd.mapEntries.map { case (key, mapEntryAdd) =>
	                    injector.getInstance(mapEntryAdd.mapEntryTypeDesc.clazz.asInstanceOf[Class[AnyRef]]) match {
	                      case Left(error) => Left(error)
	                      case Right(entryInstance) => Right((key, entryInstance, mapEntryAdd.mapEntryTypeDesc, mapEntryAdd.mapEntryGNode))
	                    }
	                  }
                   
	                  val mapEntries = entryResults.flatMap{ 
	                        case Left(error) => None
	                        case Right(entry) => Some(entry)
	                  }
	                  
	                  mapAdd.mapDesc.getFromType(instance) match {
	                    case null =>
	                      List(MapCannotBeNull(typeDesc, mapDesc))
	                    case mapWrapper =>
	                      if (mapWrapper.empty) {
	                        //Add all the instances into the map as a creation
	                        mapAdd.mapDesc.create(instance, mapEntries.map{ case (key, entryInstance, entryTypeDesc, entryGNode) =>
	                          (key, entryInstance)
	                        })
                          //Now initialize all of those instances
	                      	mapEntries.flatMap{case (key, entryInstance, entryTypeDesc, entryGNode) =>
	                      	  initializeType(entryTypeDesc, entryInstance, entryGNode) match {
	                      	    case Right(_) => Nil
	                      	    case Left(errors) => List(errors)
	                      	  }
	                      	}
	                      } else {
	                        List(MapMustStartEmpty(typeDesc, mapDesc))
	                      }
	                  }
	                  
	              	case mapBlank : MapBlank =>
	                  mapBlank.mapDesc.get(instance) match {
	                    case null =>
	                      List(MapCannotBeNull(typeDesc, mapDesc))
	                    case mapInstance =>
	                      //Do nothing!
	                      Nil
	                  }
	              }
	          }
	      }
	   }
     results match {
     	 case Nil => Right(instance)
       case errors => Left(TypeErrors(typeDesc, errors))
     }
    }
  }
  
  /** Calls the start method recursively on the type and all of it's children/mapChildren
   */
  def startType(typeDesc : TypeDescriptor, instance : AnyRef) : Option[Error] = {
    try {
	    typeDesc.fields.foreach { fieldDesc =>
	      fieldDesc match {
		      case attrDesc : AttributeDescriptor =>
		        //Do Nothing
		        //Attributes don't get started
		      case updaterDesc : UpdaterDescriptor =>
		        try {
		        	updaterDesc.get(instance).handle(UpdateTypes.Start())
		        } catch {
		          case e => 
		            throw new Exception("Start Unable to start updaterDesc[%s]" format updaterDesc,e)
		        }
		      case childDesc : ChildDescriptor =>
		        childDesc.getChildDescriptor(instance) match {
		          case None =>
		            throw new Exception("Start Unable to find descriptor for child[%s]" format childDesc)
		          case Some((childInstance, childTypeDescriptor)) =>
		            startType(childTypeDescriptor, childInstance) match {
		              case None =>
		              case Some(error) => 
		                throw error.toException
		            }
		        }
		      case mapDesc : MapDescriptor =>
		        try {
		        	mapDesc.getFromType(instance).entriesWithDesc.foreach{ case(mapEntryTypeDesc, (key, mapEntryInstance)) =>
		        	  try {
		        	  	startType(mapEntryTypeDesc, mapEntryInstance) match {
		        	  	  case None =>
		        	  	  case Some(error) =>
		        	  	  	throw error.toException
		        	  	}
		        	  } catch {
		        	    case e => throw new Exception("Error Starting MapType[%s,%s,%s]" format (mapDesc, mapEntryTypeDesc, key, mapEntryInstance), e)
		        	  }
		        	}
		        } catch {
		          case e => throw new Exception("Start Unable to scan map[%s]" format mapDesc, e)
		        }
	      }
	    }
      None
    } catch {
      case e =>
        Some(ExceptionError(typeDesc, e))
    }
  }
  
  
  
  /** 
   * Called when a type needs to be refreshed
   */
	def refreshType(typeDesc : TypeDescriptor, typeInstance : AnyRef, typeSettingsNode : Node)(implicit shell : GroovyShell) = {
	  
	  var hasError = false
	  typeDesc.fields.map { fieldDesc =>
	    fieldDesc match {
	      case attrDesc : AttributeDescriptor =>
	        refreshAttributeNode(attrDesc, typeInstance, typeSettingsNode) match {
	          case Left(error) =>
	            hasError = true
	            Left(error)
	          case Right(result) =>
	            
	        }
	      case updaterDesc : UpdaterDescriptor =>
	      	
	    }
	  }
	}
 
  /** 
   * Gets the settings value for a given attribute descriptor, taking into account default values
   */
  def getSettingsValue(attrDesc : AttributeDescriptor, typeSettingsNode : Node)(implicit shell : GroovyShell) : Either[(String, Option[Exception]), Any] = {
    typeSettingsNode.attribute(attrDesc.name) match {
      case null if (attrDesc.defaultValue.isDefined) =>
        val defaultValueStr = attrDesc.defaultValue.get
        try {
        	Right(shell.evaluate(defaultValueStr))
        } catch {
          case e : Exception =>
          	Left("Exception while computing default value", Some(e))
        }
      case null if (attrDesc.required) =>
        Left("Missing required Attribute", None)
      case value =>
        Right(value)
    }
  }
  
  /** 
   * Maps an attribute into an initialize action
   */
  def initializeAttributeNode(attrDesc : AttributeDescriptor, 
                         typeSettingsNode : Node)(implicit shell : GroovyShell, brewverter : Brewverter) : Either[AttributeError, AttributeSet] = {
    getSettingsValue(attrDesc, typeSettingsNode) match {
      case Right(newValue) =>
        //Ensure that the new value is assignable
        brewverter.convert(newValue, attrDesc.field.getType) match {
          case Left(error) => Left(AttributeTypeError(attrDesc, newValue))
          case Right(newValue) => Right(AttributeSet(attrDesc, newValue))
        }
      case Left((errorString, exception)) =>
        Left(AttributeSettingsError(attrDesc, errorString, exception))
    }
  }
  
  /** 
   * Maps an attribute into a refresh action
   */
  def refreshAttributeNode(attrDesc : AttributeDescriptor, 
                       typeInstance : AnyRef, 
                       typeSettingsNode : Node)(implicit shell : GroovyShell) : Either[AttributeError, AttributeRefreshSuccess] = {
    val currentValue = attrDesc.get(typeInstance)
    getSettingsValue(attrDesc, typeSettingsNode) match {
      case Right(newValue)=>
        if (newValue != currentValue) {
          if (attrDesc.immutable) {
            Left(AttributeMutationError(attrDesc, currentValue, newValue))
          } else {
            //Ensure that the new value is assignable
            if (attrDesc.field.getType.isAssignableFrom(newValue.asInstanceOf[AnyRef].getClass)) {
            	Right(AttributeChanged(attrDesc, Some(currentValue), newValue))
            } else {
            	Left(AttributeTypeError(attrDesc, newValue))
            }
          }
        } else {
          Right(AttributeUnchanged(attrDesc, currentValue))
        }
      case Left((errorString, exception)) =>
        Left(AttributeSettingsError(attrDesc, errorString, exception))
    }
  }



  /** 
   * Maps a child descriptor to an update action
   */
  def initializeChildNode(childDesc : ChildDescriptor,
  								 typeSettingsNode : Node) : Either[ChildError, ChildInitSuccess] = {
    //Try to get a the node with this name
    getSettingsChildSingle(typeSettingsNode, childDesc.name) match {
      case Left(errorMessage) =>
        Left(ChildSettingsNodeError(childDesc, errorMessage))
      case Right(Some(containerGNode)) =>
        //Get the one and only node that should be in the container
        getSettingsChildSingle(containerGNode) match {
          case Left(errorMessage) =>
            Left(ChildSettingsNodeError(childDesc, errorMessage))
          case Right(None) =>
            if (childDesc.required) {
              Left(ChildRequiredError(childDesc))
            } else {
              //Set the instance as empty
              //This is probably a bad idea
              Right(ChildBlank(childDesc))
            }
          case Right(Some(childTypeGNode)) =>
            //Get the type descriptor for the settings node
            val name = childTypeGNode.name.asInstanceOf[String]
            childDesc.nameMap.get(name) match {
              case Some(childTypeDesc) =>
                Right(ChildAdd(childDesc, childTypeDesc, childTypeGNode))
              case None =>
                Left(ChildNameNotFoundError(childDesc, name))
            }
        }
      case Right(None) =>
        if (childDesc.required) {
          Left(ChildRequiredError(childDesc))
        } else {
          Right(ChildBlank(childDesc))
        }
    }
  }
  
  
  /** 
   * Maps a child descriptor to a refrest action
   */
  def refreshChildNode(childDesc : ChildDescriptor,
  								 typeInstance : AnyRef,
  								 typeSettingsNode : Node) : Either[ChildError, ChildRefreshSuccess] = {
    initializeChildNode(childDesc, typeSettingsNode) match {
      case Left(e)  =>
        Left(e)
      case Right(res) =>
        val currentChild = childDesc.get(typeInstance).asInstanceOf[AnyRef]
        res match {
          case add : ChildAdd =>
          	if (currentChild.getClass == add.childTypeDesc.clazz) {
          	  Right(ChildRefresh(childDesc, currentChild, add.childTypeDesc, add.groovyNode))
          	} else {
          	  //We need to remove the old child and replace it with the new one
          	  if (childDesc.immutable) {
          	    Left(ChildMutatedError(childDesc))
          	  } else {
          	    //Ok we should be good to replace it with a new instance
          	    Right(ChildReplace(childDesc : ChildDescriptor, currentChild, add.childTypeDesc, add.groovyNode))
          	  }
          	}            
          case blank : ChildBlank =>
            if (childDesc.immutable) {
              currentChild match {
                case null => Right(blank)
                case _ => Left(ChildMutatedError(childDesc))
              }
            } else {
            	Right(ChildRemove(childDesc, currentChild))
            }
        }
    }
  }
  
  /** 
   * Maps a map node to a initialization action
   */
  def initializeMapNode(mapDesc : MapDescriptor,
  											typeSettingsNode : Node) : Either[MapError, MapInitSuccess] = {

    getSettingsChildSingle(typeSettingsNode, mapDesc.name) match {
      case Left(errorMessage) =>
        Left(MapSettingsNodeError(mapDesc, errorMessage))
      case Right(Some(containerGNode)) =>
        val entryGNodes = getSettingsChildren(containerGNode) 
      	val size = entryGNodes.size 
        if (size < mapDesc.minSize) {
          Left(MapTooSmallError(mapDesc, size, mapDesc.minSize))
        } else {
          entryGNodes match {
            case Nil =>
              Right(MapBlank(mapDesc))
            case entryGNodes =>
              var hasEntryErrors : Boolean = false
              val entries = entryGNodes map { entryGNode =>
              	val name = entryGNode.name.asInstanceOf[String]
                val res = mapDesc.nameMap.get(name) match {
                  case Some(mapEntryTypeDesc) =>
                    entryGNode.attribute("$key") match {
                      case null =>
                        Left(MapEntryMissingKeyError(mapDesc, mapEntryTypeDesc))                        
                      case key =>
                        Right(MapEntryAdd(mapEntryTypeDesc, key, entryGNode))
                    }
                  case None =>
                    Left(MapEntryNameNotFoundError(mapDesc, name))
                }
                hasEntryErrors = hasEntryErrors || res.isLeft
                res
              }
              if (hasEntryErrors) {
                Left(MapEntriesError(mapDesc, entries))
              } else {
                Right(MapAdd(mapDesc, Map() ++ entries.map { case Right(entry) => 
                  (entry.key, entry)
                }))
              }
          }
        }
      case Right(None) =>
        if (mapDesc.minSize > 0) {
          Left(MapTooSmallError(mapDesc, 0, mapDesc.minSize))
        } else {
          Right(MapBlank(mapDesc))
        }
    }
  }
  
  /** 
   * Maps a map node to a refresh action
   */
  def refreshMapNode(mapDesc : MapDescriptor,
  									 typeInstance : AnyRef,
  									 typeSettingsNode : Node) : Either[MapError, MapRefreshSuccess] = {
  	initializeMapNode(mapDesc, typeSettingsNode) match {
  	  case Left(e) => Left(e)
      case Right(res) =>
        val currentMapWrapper = mapDesc.getFromType(typeInstance)
        res match {
          case blank : MapBlank =>
            if (mapDesc.immutable && !currentMapWrapper.empty) {
              Left(MapMutatedError(mapDesc))
            } else {
              //TODO Iterate over the currentMapWrapper 
              //and get all the nodes to remove
              var hasError = false
              val removes = currentMapWrapper.entries.map { case (key, entryInstance) =>
                val entryClass = entryInstance.asInstanceOf[AnyRef].getClass
                mapDesc.classMap.get(entryClass) match {
                  case Some(mapEntryDesc) =>
                    Right(MapEntryRemove(mapEntryDesc, key, entryInstance))
                  case None =>
                    hasError = true
                    Left(MapEntryTypeNotFoundError(mapDesc, entryClass))
                }
              }
              
              if (hasError) {
                Left(MapEntriesError(mapDesc, removes))
              } else {
                Right(MapUpdate(mapDesc,Map()++ removes.map{case Right(entry) => (entry.key, entry)}))
              }
            }
          case add : MapAdd =>
            val newSize = add.mapEntries.size
            val oldSize = currentMapWrapper.size
            if (mapDesc.immutable && oldSize != newSize) {
              Left(MapMutatedError(mapDesc))
            } else {
              var hasError = false
              val entryActions = add.mapEntries.map { case (_,newMapEntry) =>
                  currentMapWrapper.get(newMapEntry.key) match {
                    case Left(errorMessage) =>
                      hasError = true
                      Left(MapSettingsNodeError(mapDesc, errorMessage))
                    case Right(newOpt) =>
                      newOpt match {
                        case Some(currentEntryInstance) =>
                          val entryClass = currentEntryInstance.asInstanceOf[AnyRef].getClass
                          mapDesc.classMap.get(entryClass) match {
                            case Some(currentEntryTypeDesc) =>
                              if (currentEntryTypeDesc.clazz == newMapEntry.mapEntryTypeDesc.clazz) {
                                Right(MapEntryRefresh(currentEntryTypeDesc, newMapEntry.key, newMapEntry.mapEntryGNode, currentEntryInstance))
                              } else {
                                if (mapDesc.immutable) {
                                  hasError = true
                                  Left(MapMutatedError(mapDesc))
                                } else {
                                	Right(MapEntryReplace(currentEntryTypeDesc, newMapEntry.key, newMapEntry.mapEntryGNode, currentEntryInstance))
                                }
                              }
                            case None =>
                              hasError = true
                              Left(MapEntryTypeNotFoundError(mapDesc, entryClass))
                          }
                        case None =>
                        	if (mapDesc.immutable) {
                            hasError = true
                            Left(MapMutatedError(mapDesc))
                          } else {
                          	Right(newMapEntry)
                          }
                      }
                  }
              }.toList
              val removeActions = currentMapWrapper.entries.flatMap { case (oldKey, oldInstance) =>
                add.mapEntries.get(oldKey) match {
                  case None =>
                    if (mapDesc.immutable) {
                      List(Left(MapMutatedError(mapDesc)))
                    } else {
                      val entryClass = oldInstance.asInstanceOf[AnyRef].getClass
                      mapDesc.classMap.get(entryClass) match {
                        case Some(mapEntryDesc) =>
                          List(Right(MapEntryRemove(mapEntryDesc, oldKey, oldInstance)))
                        case None =>
                          hasError = true
                          List(Left(MapEntryTypeNotFoundError(mapDesc, entryClass)))
                      }
                    }
                  case _ => 
                    //Covered above
                    Nil
                }
              }.toList
              if (hasError) {
                Left(MapEntriesError(mapDesc, (entryActions ::: removeActions)))
              } else {
                Right(MapUpdate(mapDesc, Map()++(entryActions ::: removeActions).map{case Right(entryAction)=>(entryAction.key, entryAction)})) 
              }
            }
        }
  	}
  }
  
}
