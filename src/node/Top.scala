package spade.node

import spade._
import spade.params._
import scala.collection.mutable._

abstract class Top(param:TopParam)(implicit design:Design) extends Module
