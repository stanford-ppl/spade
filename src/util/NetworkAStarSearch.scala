package spade.util

import spade.node._

trait NetworkAStarSearch extends prism.mapper.UniformCostGraphSearch[Bundle[_], (Port[_<:PinType], Port[_<:PinType]), Int] {

  val spademeta:SpadeMetadata
  import spademeta._

  type PT = Port[_<:PinType]
  type Action = (PT, PT)
  type C = Int

  val cnu:Numeric[Int] = implicitly[Numeric[Int]]

  def heuristic(end:Routable)(newState:Bundle[_]):Int = {
    val next = routableOf(newState).get
    if (next.isInstanceOf[ArgFringe] || end.isInstanceOf[ArgFringe]) return 0
    val List(cx, cy) = indexOf(next)
    val List(ex, ey) = indexOf(end)
    Math.abs(cx - ex) + Math.abs(cy - ey)
  }

  def advance(
    startTails:List[PT],
    tailToHead:Edge => List[Edge],
    heuristic:Bundle[_] => C,
    maxCost:Int
  )(
    state:Bundle[_], 
    backPointers:BackPointer, 
    pastCost:C
  ):Seq[(Bundle[_], Action, C)] = {

    if (maxCost>0 && pastCost>maxCost) return Nil
    dbgblk(2, s"advance(state=${quote(state)} pastCost=$pastCost)") {
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
              val newState = head2.src.asInstanceOf[Bundle[_<:PinType]]
              (newState, (tail2, head2), 1 + heuristic(newState))
            }
          }
        case rt:Routable =>
          /*
           *   +----------+      +----------+
           *   |    tails +----->|heads     +
           *   |  curr    +----->|          |
           *   +----------+      +----------+
           * */
          startTails.flatMap { tail =>
            tailToHead(tail.external).map { headedge =>
              val head = headedge.src.asInstanceOf[PT]
              val newState = head.src.asInstanceOf[Bundle[_<:PinType]]
              (newState, (tail, head), 1 + heuristic(newState))
            }
          }
      }
    }
  }

  override def quote(n:Any) = n match {
    case (state, action, cost) => (quote(state), quote(action), cost).toString
    case (tail, head) => (quote(tail), quote(head)).toString
    case n:Bundle[_] => s"${quote(routableOf(n).get)}.${n}[${n.bct.runtimeClass.getSimpleName}]"
    case n => super.quote(n)
  }

}
