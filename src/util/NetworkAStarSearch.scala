package spade.util

import spade.node._

trait NetworkAStarSearch extends prism.mapper.UniformCostGraphSearch[Bundle[_], (Port[_<:PinType], Port[_<:PinType]), Int] { 

  val spademeta:SpadeMetadata
  import spademeta._

  type PT = Port[_<:PinType]
  type Action = (PT, PT)
  type C = Int

  implicit val cnu:Numeric[Int] = implicitly[Numeric[Int]]

  def heuristic(next:Routable, end:Routable):Int = {
    if (next.isInstanceOf[ArgFringe] || end.isInstanceOf[ArgFringe]) return 0
    val List(cx, cy) = indexOf(next)
    val List(ex, ey) = indexOf(end)
    Math.abs(cx - ex) + Math.abs(cy - ey)
  }

  def heuristic(port:PT, end:Option[Routable]):Int = {
    end.fold(0) { end => 
      val next = routableOf(port).get
      heuristic(next, end)
    }
  }

  def advance(
    start:Routable, 
    end:Option[Routable], 
    startTails:List[PT], 
    tailToHead:Edge => List[Edge]
  )(
    state:Bundle[_], 
    backPointers:BackPointer, 
    pastCost:C
  ):Seq[(Bundle[_], Action, C)] = {
    routableOf(state).get match {
      case rt:SwitchBox =>
        /*
         *   +----------+      +----------+       +----------+
         *   |        t1+----->|h1      t2+------>|h2        |
         *   |          |      |  curr    |       |          |
         *   +----------+      +----------+       +----------+
         * */
        val (_, (tail1, head1), _) = backPointers(state)
        tailToHead(head1.internal).flatMap { tail2ic =>
          val tail2 = tail2ic.src.asInstanceOf[PT]
          tailToHead(tail2.external).map { head2edge =>
            val head2 = head2edge.src.asInstanceOf[PT]
            (head2.src.asInstanceOf[Bundle[_<:PinType]], (tail2, head2), 1 + heuristic(head2, end))
          }
        }
      case rt:Routable if rt == start => // Start
        /*
         *   +----------+      +----------+
         *   |    tails +----->|heads     +
         *   |  curr    +----->|          |
         *   +----------+      +----------+
         * */
        startTails.flatMap { tail =>
          tailToHead(tail.external).map { headedge =>
            val head = headedge.src.asInstanceOf[PT]
            (head.src.asInstanceOf[Bundle[_<:PinType]], (tail, head), 1 + heuristic(head, end))
          }
        }
      case _ => Nil
    }
  }

}
