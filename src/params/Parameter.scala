package spade.params

import prism.enums._
import prism.node._

abstract class Parameter extends ProductNode[Parameter](None) with ProductAtom[Parameter] with Serializable {
  override val id = this.hashCode & 0xfffffff

  def newIn = ParameterInput(this)
  def out = ParameterOutput(this)
}

case class ParameterInput(src:Parameter) extends Input[Parameter] {
  type A = Parameter
  override val id = this.hashCode & 0xfffffff
}
case class ParameterOutput(src:Parameter) extends Output[Parameter] {
  type A = Parameter
  override val id = this.hashCode & 0xfffffff
}

trait SpadeParam extends Parameter{
  lazy val wordWidth = 32
  lazy val numLanes = 16
  lazy val clockFrequency:Int = 1000000000 //Hz
}

trait PreLoadSpadeParam extends SpadeParam {
  override lazy val numLanes = Factory.plasticineConf.lanes
}
