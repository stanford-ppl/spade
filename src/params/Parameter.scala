package spade.params

import prism.enums._
import prism.node._

trait Parameter extends Serializable

trait SpadeParam extends Parameter{
  lazy val wordWidth = 32
  lazy val numLanes = 16
  lazy val clockFrequency:Int = 1000000000 //Hz
}

trait PreLoadSpadeParam extends SpadeParam with Parameter {
  override lazy val numLanes = Factory.plasticineConf.lanes
}

