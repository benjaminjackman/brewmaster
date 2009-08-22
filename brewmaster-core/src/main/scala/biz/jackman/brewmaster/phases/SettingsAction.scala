package biz.jackman.brewmaster.phases

trait SettingsAction {
 type ReturnType
 def perform : ReturnType
}
