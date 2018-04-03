package spade.node

abstract class SpadeNode(implicit design:SpadeDesign) extends Node[SpadeNode] with SpadeCollector { self =>
  val id = design.nextId

  type N = SpadeNode
  type P = Module
  type A = Pin[_]

  def qindex = {
    import design.spademeta._
    s"${nameOf.get(this).getOrElse(className)}${id}${indexOf.get(this).fold("")(indices => s"[${indices.mkString(",")}]")}"
  }
}

