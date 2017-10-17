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

  type N = Routable
  type C = Int
  type PIO = IO[_<:PortType, Module] 
  type PI = Input[_<:PortType, Module]
  type PO = Output[_<:PortType, Module]
  type Edge = (IO[_<:PortType, Module], IO[_<:PortType, Module])
  type FE = (PO, PI) 
  type BE = (PI, PO)
  type M <: SpadeMap

  def setConfig[E<:Edge](map:M, path:List[(N, E)]):M = {
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
      mp = mp.setFI(in, out).asInstanceOf[M] //TODO: fix this?
      out match {
        case out:GlobalOutput[_,_] =>
          outFrom.foreach { outFrom => // Config SwitchBox
            mp = mp.setFI(out.ic, outFrom.ic).asInstanceOf[M]
          }
        case _ =>
      }
    }
    mp
  }

  def advance[PI,PO](
    tails:N => Iterable[PO], 
    heads:PO => Iterable[PI],
    src:PI => N,
    start:N
  )(n:N,c:C):Iterable[(N, (PO,PI))] = {
    type X = (N,(PO,PI))
    n match {
      case n:Controller if n != start => Nil
      case n =>
        tails(n).flatMap[X, Iterable[X]]{ out => 
          heads(out).map[X, Iterable[X]]{ in => (src(in), (out, in)) } 
        }
    }
  }

  def advance(
    tails:N => Iterable[PO], 
    start:N
  )(n:N,c:C):Iterable[(N, (PO,PI))] = {

    advance[PI,PO](
      tails   = tails,
      heads   = (o:PO) => o.fanOuts,
      src     = (i:PI) => i.src.asInstanceOf[N],
      start   = start
    )(n,c)
  }

  def uniformCostSearch[A<:Edge](
    start:N, 
    end:N,
    advance:(N,C) => Iterable[(N, A, C)],
    map:M,
    finPass: (M,List[(N,A)],C) => M,
    logger:Option[Logger] = None
  ):Either[PIRException, M] = {
    def fp(route:List[(N,A)], cost:C):M = {
      finPass(setConfig(map, route),route,cost)
    }
    uniformCostSearch ( // defined in pirc.util.GraphSearch
      start    = start,
      isEnd    = { (n:N) => n == end },
      zeroCost = 0,
      sumCost  = { (a:C, b:C) => a + b },
      advance  = advance,
      quote = spade.util.quote _,
      finPass = fp _,
      logger = logger
    )
  }

  def simpleCostSearch[A<:Edge](
    start:N, 
    end:N,
    advance:(N,C) => Iterable[(N, A)],
    map:M,
    finPass: (M,C) => M,
    logger:Option[Logger] = None
  ):Either[PIRException,M] = {
    def simpleCostAdvance(n:N,c:C):Iterable[(N, A, C)] = {
      advance(n,c).map { case (n, a) => (n, a, 1) }
    }
    def fp(mp:M, route:List[(N,A)], cost:C) = {
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

  def uniformCostSpan (
    start:N, 
    advance:(N,C) => Iterable[(N, C)], 
    logger:Option[Logger]
  ):Iterable[(N,C)] = {
    uniformCostSpan (
      start    = start,
      zeroCost = 0,
      sumCost  = { (a:C, b:C) => a + b },
      advance  = advance,
      quote    = spade.util.quote _,
      logger   = logger
    )
  }

}
