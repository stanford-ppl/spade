package spade
package node

case class SwitchParam(
  connection:SwitchConnection=CrossBarSwitchConnection
) extends Parameter
