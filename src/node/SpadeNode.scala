package spade.node

import spade.params._
import prism.node._
import prism.enums._

abstract class SpadeNode(val id:Int) extends Node[SpadeNode] { self =>
  def this()(implicit design:Design) = this(design.nextId)
  type N = SpadeNode
  type P = Module
  type A = Bundle[_]
}

