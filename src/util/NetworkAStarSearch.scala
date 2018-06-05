package spade
package util

import spade.node._

trait NetworkAStarSearch extends prism.mapper.UniformCostGraphSearch[Bundle[_], (Port[_<:PinType], Port[_<:PinType]), Int] {

  val spademeta:SpadeMetadata
  import spademeta._

  type PT = Port[_<:PinType]
  type Action = (PT, PT)
  type C = Int

  val cnu:Numeric[Int] = implicitly[Numeric[Int]]

  def designS:SpadeDesign
  lazy val heuristicCost: Routable => (Bundle[_] => Int) = designS match {
    case designS if isMesh(designS) => meshHeuristicCost _
    case designS if isCMesh(designS) => meshHeuristicCost _
    case designS if isTorus(designS) => torusHeuristicCost _
  }

  def meshHeuristicCost(end:Routable)(newState:Bundle[_]):Int = {
    val next = routableOf(newState).get
    zipMap(indexOf.get(next), indexOf.get(end)) { case (List(nx, ny), List(ex, ey)) =>
      // Manhattan distance between two points
      Math.abs(nx - ex) + Math.abs(ny - ey)
    }.getOrElse(0)
  }

  lazy val maxWidth = designS.top match {
    case top:StaticGridTop => top.sbrx - top.sblx
    case top:DynamicGridTop => top.rtrx - top.rtlx
  }
  lazy val maxHeight = designS.top match {
    case top:StaticGridTop => top.sbuy - top.sbby
    case top:DynamicGridTop => top.rtuy - top.rtby
  }
  def torusHeuristicCost(end:Routable)(newState:Bundle[_]):Int = {
    val next = routableOf(newState).get
    zipMap(indexOf.get(next), indexOf.get(end)) { case (List(nx, ny), List(ex, ey)) =>
      // Manhattan distance between two points with wrap around
      val xdelt = Math.abs(nx - ex)
      val ydelt = Math.abs(ny - ey)
      val xdist = Math.min(xdelt, maxWidth - xdelt)
      val ydist = Math.min(ydelt, maxHeight - ydelt)
      xdist + ydist
    }.getOrElse(0)
  }

  def advance(
    startTails:List[PT],
    tailToHead:Edge => List[(Edge,C)],
    heuristicCost:Bundle[_] => C,
    maxCost:Int
  )(
    state:Bundle[_], 
    backPointers:BackPointer, 
    pastCost:C
  ):Seq[(Bundle[_], Action, C)] = {

    if (maxCost>0 && pastCost>maxCost) return Nil
    dbgblk(s"advance(state=${quote(state)} pastCost=$pastCost)",buffer=false) {
      routableOf(state).get match {
        case _:SwitchBox | _:Router =>
          /*
           *   +----------+      +----------+       +----------+
           *   |        t1+----->|h1      t2+------>|h2        |
           *   |          |      |  curr    |       |          |
           *   +----------+      +----------+       +----------+
           * */
          val (_, (tail1, head1), _) = backPointers(state)
          tailToHead(head1.internal).flatMap { case (tail2ic, cost1) =>
            val tail2 = tail2ic.src.asInstanceOf[PT]
            tailToHead(tail2.external).map { case (head2edge, cost2) =>
              val head2 = head2edge.src.asInstanceOf[PT]
              val newState = head2.src.asInstanceOf[Bundle[_<:PinType]]
              (newState, (tail2, head2), (cost1 + cost2) + heuristicCost(newState))
            }
          }
        case _:Routable =>
          /*
           *   +----------+      +----------+
           *   |    tails +----->|heads     +
           *   |  curr    +----->|          |
           *   +----------+      +----------+
           * */
          startTails.flatMap { tail =>
            tailToHead(tail.external).map { case (headedge, cost) =>
              val head = headedge.src.asInstanceOf[PT]
              val newState = head.src.asInstanceOf[Bundle[_<:PinType]]
              (newState, (tail, head), cost + heuristicCost(newState))
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
