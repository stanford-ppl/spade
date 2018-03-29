package spade.util

import spade._
import spade.node._

import prism._
import prism.util._
import prism.traversal._

import scala.language.postfixOps
import scala.language.existentials

abstract class NetworkAStarSearch[B<:PinType:ClassTag:TypeTag] extends UniformCostGraphSearch[Bundle[B], (Port[B], Port[B]), Int] { 
  val spademeta: SpadeMetadata
  import spademeta._

  implicit def arch:Spade

  type Edge = (Port[B], Port[B])
  type C = Int

  def quote(n:Any) = n match {
    case n:SpadeNode => n.qindex
    case n => n.toString
  }

  def heuristic(next:Routable, end:Routable):Int = {
    if (next.isInstanceOf[ArgFringe] || end.isInstanceOf[ArgFringe]) return 0
    val List(cx, cy) = indexOf(next)
    val List(ex, ey) = indexOf(end)
    Math.abs(cx - ex) + Math.abs(cy - ey)
  }

  def heuristic(port:Port[B], end:Option[Routable]):Int = {
    end.fold(0) { end => 
      val next = routableOf(port).get
      heuristic(next, end)
    }
  }

  def advance(forward:Boolean, end:Option[Routable])(state:Bundle[B], backPointers:BackPointer, pastCost:C):Seq[(Bundle[B], Edge, C)] = {
    routableOf(state).get match {
      case rt:SwitchBox =>
        /*
         *   +----------+      +----------+       +----------+
         *   |        t1+----->|h1      t2+------>|h2        |
         *   |          |      |  curr    |       |          |
         *   +----------+      +----------+       +----------+
         * */
        val (_, (tail1, head1), _) = backPointers(state)
        head1.internal.connected.flatMap { tail2ic =>
          val tail2 = tail2ic.src.asInstanceOf[Port[B]]
          tail2.connected.map { head2 =>
            (head2.src.asInstanceOf[Bundle[B]], (tail2, head2), 1 + heuristic(head2, end))
          }
        }
      case rt:Routable if !backPointers.contains(state) => // Start
        /*
         *   +----------+      +----------+
         *   |    tails +----->|heads     +
         *   |  curr    +----->|          |
         *   +----------+      +----------+
         * */
        val tails = if (forward) state.outputs else state.inputs
        tails.flatMap { tail =>
          tail.connected.map { head =>
            (head.src.asInstanceOf[Bundle[B]], (tail, head), 1 + heuristic(head, end))
          }
        }
      case _ => Nil
    }
  }

  def search(
    start:Routable, 
    end:Routable,
    forward:Boolean,
    logger:Option[Logging]
  ):EOption[Route] = {
    uniformCostSearch(
      start=bundleOf[B](start).get, 
      isEnd=(n:Bundle[B]) => routableOf(n).get == end,
      advance=advance(forward, Some(end))_,
      logger=logger
    )
  }

  def span(
    start:Routable, 
    forward:Boolean,
    logger:Option[Logging]
  ):Seq[(Routable,C)] = {
    uniformCostSpan(
      start=bundleOf[B](start).get, 
      advance=advance(forward, None)_,
      logger=logger
    ).map { case (bundle, cost) => (routableOf(bundle).get, cost) }
  }


}
