package biz.jackman.brewmaster.annotations

import phases.SettingsAction
import phases.PhaseErrorCodes.Error

/** The Infusable trait allows for classes to process
 * settings actions in the scope of their own threading 
 * models. The settings actions for that type and all
 * child types will then be processed in the context of
 * whereever SettingsAction.perform is called.
 * <p>
 * This trait thus allows for safely keeping the locking structure
 * required by the class when touching it's mutable fields secure.
 * A locking strategy that is this strict may not always be required.
 */
trait Infusable {
	def infuse(action : SettingsAction) : Either[Error, AnyRef]
}
