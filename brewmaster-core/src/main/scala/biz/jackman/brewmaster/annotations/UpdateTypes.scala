package biz.jackman.brewmaster.annotations

sealed trait UpdateType

object UpdateTypes {
  
  /** Called when a class is having it's settings
   * inserted
   */
  case class Initialize() extends UpdateType
 
  /** Called after all classes have had their settings
  * inserted. This is the time when it safe to start threads,
  * and preserve the guarantees of the java memory model for
  * variable visibility.
  */
	case class Start() extends UpdateType
 
  /** Called when a class is having it's settings refreshed.
   * Perhaps a custom field that relies on a settings based field
   * needs to have a value set, in that event then the field should
   * update it's settings
   */
  case class Refresh() extends UpdateType
  
  /** Called if the refresh step fails and clients need to revert
   * to their previous configuration. Notice that a failure can occurr
   * at either the refresh or start-post-refresh stage, in either event
   * the callee should perform any rollbacks that are needed. The settings
   * fields in the class will be being reverted back to their previous values
   */
  case class Undo() extends UpdateType
  
  /** Called when an object can be safely removed from the tree. This happens 
   * in a few cases:
   * 1 it is removed from it's containing map
   * 2 an instance with the same key but a different type is placed into it's map
   * 3 an instance with a different type is placed into it's child container
   * 4 the program is being shutdown
   */
  case class Remove() extends UpdateType
}
