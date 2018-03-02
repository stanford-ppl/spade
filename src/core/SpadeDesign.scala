package spade

import spade._
import spade.node._
import spade.params._

case class SpadeDesign(param:DesignParam) extends prism.node.Design {
  val spademeta = new SpadeMetadata
  val top:Top = Factory.create(param)
}
