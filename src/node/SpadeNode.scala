package spade.node

import spade.params._
import prism.node._

abstract class SpadeNode(implicit design:Design) extends Node[SpadeNode] { self =>
  val id = design.nextId

  type N = SpadeNode
  type P = Module
  type A = Bundle[_]
}

