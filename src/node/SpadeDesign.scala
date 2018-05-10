package spade
package node

case class SpadeDesign(param:TopParam)(implicit compiler:Spade) extends prism.node.Design {
  val spademeta = new SpadeMetadata
  val top:Top = Factory.logger.withOpen(compiler.outDir, s"top.log", append=false) {
    Factory.create(param)
  }
}
