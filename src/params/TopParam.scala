package spade.node

import prism.node._

trait TopParam extends Parameter {
  lazy val wordWidth = 32
  lazy val vecWidth = 16
  lazy val clockFrequency:Int = 1000000000 //Hz
  val busWithReady:Boolean // TODO: make this a parameter of network
}

trait PreLoadTopParam extends TopParam {
  override lazy val vecWidth = Factory.plasticineConf.lanes
}
