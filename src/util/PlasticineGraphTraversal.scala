package spade.util

import spade._
import spade.node._

import prism._
import prism.util._
import prism.traversal._

import scala.language.postfixOps
import scala.language.existentials

trait PlasticineUniformCostSearch[B<:PinType] extends UniformCostGraphSearch[Bundle[B], (Port[B], Port[B]), Int] {

  implicit def arch:Spade

  type Edge = (Port[B], Port[B])
  type C = Int

  def quote(n:Any) = n match {
    case n:SpadeNode => n.qindex
    case n => n.toString
  }

  //def advance(state:Bundle[B], backPointers:BackPointer, cost:C):Seq[(Bundle[B], Edge, C)] = {
    //val (_, edge, _) = backPointers(state)
    //val (tail, head) = edge
    //head.internal.connected
  //}

}
