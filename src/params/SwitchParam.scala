package spade.params

import spade.node._

case class SwitchParam(
  connection:SwitchConnection=CrossBarSwitchConnection
) extends Parameter
