package spade.traversal

import spade._
import spade.node._
import spade.config._
import spade.util._

import pirc._
import pirc.util._
import pirc.codegen.Logger

import scala.language.postfixOps
import scala.language.existentials

trait PlasticineGraphTraversal extends UniformCostGraphSearch {

  implicit def arch:Spade

  type S = Routable
  type C = Int
  type PIO = IO[_<:PortType, Module] 
  type PI = Input[_<:PortType, Module]
  type PO = Output[_<:PortType, Module]
  type Edge = (IO[_<:PortType, Module], IO[_<:PortType, Module])
  type FE = (PO, PI) 
  type BE = (PI, PO)
  type M = SpadeMapLike

  def setConfig[E<:Edge](map:M, path:List[(S, E)]):M = {
    var mp = map
    path.zipWithIndex.foreach { case ((node, (io1, io2)), i) => 
      val (out, in, outFrom) = (io1, io2) match {
        case (out:PO, in:PI) => 
          val outFrom = out.src match {
            case sb:SwitchBox => Some(path(i-1)._2._2.asInput.asGlobal)
            case _ => None
          }
          (out, in, outFrom)
        case (in:PI, out:PO) => (out, in)
          val outFrom = out.src match {
            case sb:SwitchBox => Some(path(i+1)._2._1.asInput.asGlobal)
            case _ => None
          }
          (out, in, outFrom)
      }
      mp = mp.set[FIMap](in, out)
      out match {
        case out:GlobalOutput[_,_] =>
          outFrom.foreach { outFrom => // Config SwitchBox
            mp = mp.set[FIMap](out.ic, outFrom.ic)
          }
        case _ =>
      }
    }
    mp
  }

  def advance[PH,PT](
    tails:(S, Option[PH]) => Seq[PT], 
    heads:PT => Seq[PH],
    src:PH => S,
    start:S
  )(n:S, prevEdge:Option[(PT,PH)], c:C):Seq[(S, (PT,PH))] = {
    type X = (S,(PT,PH))
    n match {
      case n:Controller if n != start => Nil
      case n =>
        val head = prevEdge.map(_._2)
        tails(n, head).flatMap[X, Seq[X]]{ out => 
          heads(out).map[X, Seq[X]]{ in => (src(in), (out, in)) } 
        }
    }
  }

  /*
   * forward advance
   * */
  def advance(
    tails:(S, Option[PI]) => Seq[PO], 
    start:S
  )(n:S, prevEdge:Option[(PO,PI)], c:C):Seq[(S, (PO,PI))] = {

    advance[PI,PO](
      tails   = tails,
      heads   = (o:PO) => o.fanOuts,
      src     = (i:PI) => i.src.asInstanceOf[S],
      start   = start
    )(n, prevEdge, c)
  }

  def uniformCostSearch[A<:Edge](
    start:S, 
    end:S,
    advance:(S, Option[A], C) => Seq[(S, A, C)],
    map:M,
    finPass: (M,List[(S,A)],C) => M,
    logger:Option[Logger] = None
  ):Either[PIRException, M] = {
    def fp(route:List[(S,A)], cost:C):M = {
      finPass(setConfig(map, route),route,cost)
    }
    def adv(n:S, backPointers:BackPointer[S,A,C], c:C):Seq[(S,A,C)] = {
      val prevEdge = backPointers.get(n).map (_._2)
      advance(n, prevEdge, c)
    }
    uniformCostSearch[S,A,C,M]( // defined in pirc.util.GraphSearch
      start    = start,
      isEnd    = { (n:S) => n == end },
      zeroCost = 0,
      sumCost  = { (a:C, b:C) => a + b },
      advance  = adv _,
      quote = spade.util.quote _,
      finPass = fp _,
      logger = logger
    )
  }

  def simpleCostSearch[A<:Edge](
    start:S, 
    end:S,
    advance:(S,Option[A],C) => Seq[(S, A)],
    map:M,
    finPass: (M,C) => M,
    logger:Option[Logger] = None
  ):Either[PIRException,M] = {
    def simpleCostAdvance(n:S, prevEdge:Option[A], c:C):Seq[(S, A, C)] = {
      advance(n,prevEdge,c).map { case (n, a) => (n, a, 1) }
    }
    def fp(mp:M, route:List[(S,A)], cost:C) = {
      finPass(mp, cost)
    }
    logger.foreach { l =>
      l.dprintln(s"start=${quote(start)} end=${quote(end)} --------------")
    }
    uniformCostSearch (
      start   = start,
      end     = end,
      advance = simpleCostAdvance _,
      map     = map,
      finPass = fp _,
      logger  = logger
    )
  }

  def uniformCostSpan[A <: Edge](
    start:S, 
    advance:(S, Option[A], C) => Seq[(S, A, C)], 
    logger:Option[Logger]
  ):Seq[(S,C)] = {
    def adv(n:S, backPointers:BackPointer[S,A,C], c:C):Seq[(S,A,C)] = {
      val prevEdge = backPointers.get(n).map (_._2)
      advance(n, prevEdge, c)
    }
    uniformCostSpan[S,A,C] (
      start    = start,
      zeroCost = 0,
      sumCost  = { (a:C, b:C) => a + b },
      advance  = adv _,
      quote    = spade.util.quote _,
      logger   = logger
    )
  }

}
