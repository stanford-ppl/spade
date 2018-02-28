package spade.pass

import spade._
import spade.node._
import scala.reflect._

trait SpadeWorld {
  implicit val nct = classTag[N]
  type N = SpadeNode
  type P = Module
  type A = Bundle[_]
}

