package spade.traversal

import spade._
import spade.node._
import spade.config._
import spade.util._

import pirc.util._
import pirc.codegen.Logger

import scala.language.postfixOps
import scala.language.existentials

trait PlasticineGraphTraversal extends GraphSearch {

  implicit def arch:Spade

  type N = Routable
  type C = Int
  type I = Input[_<:PortType, Module]
  type O = Output[_<:PortType, Module]
  type E = (IO[_<:PortType, Module], IO[_<:PortType, Module])
  type FE = (O, I) 
  type BE = (I, O)
  type M = SpadeMap

  def setConfig[Edge<:E](map:M, path:List[(N, Edge)]):M = {
    var mp = map
    path.zipWithIndex.foreach { case ((node, (io1, io2)), i) => 
      val (out, in, outFrom) = (io1, io2) match {
        case (out:O, in:I) => 
          val outFrom = out.src match {
            case sb:SwitchBox => Some(path(i-1)._2._2.asInput.asGlobal)
            case _ => None
          }
          (out, in, outFrom)
        case (in:I, out:O) => (out, in)
          val outFrom = out.src match {
            case sb:SwitchBox => Some(path(i+1)._2._1.asInput.asGlobal)
            case _ => None
          }
          (out, in, outFrom)
      }
      mp = mp.setFI(in, out)
      out match {
        case out:GlobalOutput[_,_] =>
          outFrom.foreach { outFrom => // Config SwitchBox
            mp = mp.setFI(out.ic, outFrom.ic)
          }
        case _ =>
      }
    }
    mp
  }

  def advance(
    outs:N => Iterable[O], 
    start:N
  )(n:N):Iterable[(N, FE)] = {
    type X = (N,FE)
    n match {
      case n:Controller if n != start => Nil
      case n =>
        outs(n).flatMap[X, Iterable[X]]{ out => 
          out.fanOuts.map[X, Iterable[X]]{ in => (in.src.asInstanceOf[N], (out, in)) } 
        }
    }
  }

  def inverseAdvance(
    ins:N => Iterable[I], 
    start:N, 
    map:Option[SpadeMap] = None
  )(n:N):Iterable[(N,BE)] = {
    type X = (N,BE)
    n match {
      case n:Controller if n != start => Nil
      case n:SwitchBox =>
        ins(n).flatMap[X, Iterable[X]]{ in => 
          in.fanIns.map[X, Iterable[X]]{ out => (out.src.asInstanceOf[N], (in, out)) } 
        }
    }
  }

  def search[A<:E](
    start:N, 
    end:N,
    advance:N => Iterable[(N, A, C)],
    map:M,
    finPass: (M,C) => M,
    logger:Option[Logger] = None
  ):M = {
    def fp(route:List[(N,A)], cost:C):M = {
      finPass(setConfig(map, route),cost)
    }
    search ( // defined in pirc.util.GraphSearch
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

  def simpleCostSearch[A<:E](
    start:N, 
    end:N,
    advance:N => Iterable[(N, A)],
    map:M,
    finPass: (M,C) => M,
    logger:Option[Logger] = None
  ):M = {
    def simpleCostAdvance(n:N):Iterable[(N, A, C)] = {
      advance(n).map { case (n, a) => (n, a, 1) }
    }
    logger.foreach { l =>
      l.dprintln(s"start=${quote(start)} end=${quote(end)} --------------")
    }
    search (
      start   = start,
      end     = end,
      advance = simpleCostAdvance _,
      map     = map,
      finPass = finPass,
      logger  = logger
    )
  }

  def span (
    start:N, 
    advance:(N,C) => Iterable[(N, C)], 
    logger:Option[Logger]
  ):Iterable[N] = {
    span (
      start    = start,
      zeroCost = 0,
      sumCost  = { (a:C, b:C) => a + b },
      advance  = advance,
      quote    = spade.util.quote _,
      logger   = logger
    )
  }

}
