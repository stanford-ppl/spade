package spade.traversal

import spade._
import spade.node._
import spade.config._

import pirc.util._

import scala.language.postfixOps

trait PlasticineGraphTraversal extends GraphSearch {

  implicit def arch:Spade

  type N = Routable
  type C = Int
  type I = Input[_<:PortType, Module]
  type O = Output[_<:PortType, Module]
  type FE = (O, I) 
  type BE = (I, O)

  def setConfig(map:SpadeMap, path:List[(N, FE)]):SpadeMap = {
    var mp = map
    path.zipWithIndex.foreach { case ((node, (out, in)), i) => 
      mp = mp.setFI(in, out)
      out match {
        case out:GlobalOutput[_,_] =>
          if (out.src.isInstanceOf[SwitchBox]) { // Config SwitchBox
            val to = out 
            val from = path(i-1)._2._2.asGlobal
            mp = mp.setFI(to.ic, from.ic)
          }
        case _ =>
      }
    }
    mp
  }

  def advance(outs:N => Iterable[O], start:N)(n:N):Iterable[(N, FE)] = {
    type X = (N,FE)
    n match {
      case n:SwitchBox =>
        outs(n).flatMap[X, Iterable[X]]{ out => 
          out.fanOuts.map[X, Iterable[X]]{ in => (in.src.asInstanceOf[N], (out, in)) } 
        }
      case `start` => 
        outs(n).flatMap[X, Iterable[X]]{ out => 
          out.fanOuts.map[X, Iterable[X]]{ in => (in.src.asInstanceOf[N], (out, in)) } 
        }
      case n:Controller => Nil
    }
  }

  def inverseAdvance(ins:N => Iterable[I], start:N)(n:N):Iterable[(N,BE)] = {
    type X = (N,BE)
    n match {
      case n:SwitchBox =>
        ins(n).flatMap[X, Iterable[X]]{ in => 
          in.fanIns.map[X, Iterable[X]]{ out => (out.src.asInstanceOf[N], (in, out)) } 
        }
      case `start` => 
        ins(n).flatMap[X, Iterable[X]]{ in => 
          in.fanIns.map[X, Iterable[X]]{ out => (out.src.asInstanceOf[N], (in, out)) } 
        }
      case n:Controller => Nil
    }
  }

  def search[A](
    start:N, 
    end:N,
    advance:N => Iterable[(N, A, C)]
  ):(List[(N,A)], C) = {
    search (
      start    = start,
      isEnd    = { (n:N) => n == end },
      zeroCost = 0,
      sumCost  = { (a:C, b:C) => a + b },
      advance  = advance,
      quote = spade.util.quote _
    )
  }

  def simpleCostSearch[A](
    start:N, 
    end:N,
    advance:N => Iterable[(N, A)]
  ):(List[(N,A)], C) = {
    def simpleCostAdvance(n:N):Iterable[(N, A, C)] = {
      advance(n).map { case (n, a) => (n, a, 1) }
    }
    search (
      start    = start,
      isEnd    = { (n:N) => n == end },
      zeroCost = 0,
      sumCost  = { (a:C, b:C) => a + b },
      advance  = simpleCostAdvance,
      quote = spade.util.quote _
    )
  }
}
