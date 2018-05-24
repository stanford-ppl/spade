package spade
package node

case class SpadeDesign(param:DesignParam) extends prism.node.Design {
  val spademeta = new SpadeMetadata
  val top:Top = Factory.create(param)
}
