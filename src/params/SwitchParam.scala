package spade.node

case class SwitchParam(
  connection:SwitchConnection=CrossBarSwitchConnection
) extends Parameter
