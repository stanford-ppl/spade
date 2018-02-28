package spade.node

import spade._
import spade.params._

abstract class SpadeDesign(param:DesignParam) extends SpadeNode(0) with prism.node.Design with Module {
  val spademeta = new SpadeMetadata
  implicit val design:this.type = this
}
