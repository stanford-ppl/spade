package spade.node

import spade.params._
import prism.node._

abstract class SpadeNode(implicit design:Design) extends Node[SpadeNode] with SpadeCollector { self =>
  val id = design.nextId

  type N = SpadeNode
  type P = Module
  type A = Pin[_]

  def qindex = {
    import design.spademeta._
    s"${nameOf.get(this).getOrElse(className)}${id}${indexOf.get(this).fold("")(indices => s"[${indices.mkString(",")}]")}"
  }
}

