package spade.params

import prism.enums._
import prism.node._

class Parameter extends /*ProductNode[Parameter](None) with ProductSubGraph[Parameter] with */Serializable {
}

trait SpadeParam extends Parameter{
  lazy val wordWidth = 32
  lazy val numLanes = 16
  lazy val clockFrequency:Int = 1000000000 //Hz
}

trait PreLoadSpadeParam extends SpadeParam {
  override lazy val numLanes = Factory.plasticineConf.lanes
}

