package spade.traversal

import spade._
import spade.util._
import spade.node._

import pirc._

import scala.collection.mutable
import scala.language.existentials
import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}

trait HiearchicalTraversal {

  def traverseDown(
      node:Any
    ):Unit = {
    visitDown(node).foreach { node => traverseDown(node) }
  } 

  def traverseDown[T](
    node:Any, 
    result:T
  ):T = {
    visitDown(node).foldLeft(result) { case (result, node) => traverseDown(node, result) }
  }

  def visitUp(x:Any):Iterable[Any] = x match {
    case x:Module => x.parent
    case x => Set()
  }

  def visitDown(x:Any):Iterable[Any] = x match {
    case x:Module => x.ios ++ x.children
    case x => Set()
  }

  def visitIn(x:Any):Iterable[Any] = x match {
    case x:GlobalInput[_,_] => Set() // Do not cross CU boundary
    case x:GlobalOutput[_,_] => Set(x.src,x.ic)
    case x:Input[_,_] => x.fanIns
    case x:Output[_,_] => Set(x.src)
    case x:Module => x.ins
  }

  def visitOut(x:Any):Iterable[Any] = x match {
    case x:GlobalOutput[_,_] => Set() // Do not cross CU boundary
    case x:GlobalInput[_,_] => Set(x.src) ++ x.outs
    case x:Input[_,_] => Set(x.src)
    case x:Output[_,_] => x.fanOuts
    case x:Module => x.outs
  }

  def collect[X](
    x:Any, 
    visitFunc:Any => Iterable[Any], 
    depth:Int,
    logger:Option[Logger]=None,
    visited:mutable.Set[Any] = mutable.Set.empty
  )(implicit ev:ClassTag[X]):Set[X] = {
    def f(xx:Any):Set[X] = {
      visited += x
      if (depth <= 0) Set[X]()
      else collect[X](xx, visitFunc, depth - 1, logger, visited)
    }
    logger.foreach { _.emitBSln(s"collect($x, depth=$depth)") }
    val res = x match {
      case x:X => Set[X](x)
      case x:Iterable[_] => x.flatMap(f).toSet
      case x if visited.contains(x) => Set[X]()
      case x => f(visitFunc(x))
    }
    logger.foreach { l =>
      l.dprintln(res)
      l.emitBEln
    }
    res
  }

  def collectUp[X](
    x:Any, 
    visitFunc:Any => Iterable[Any]=visitUp, 
    depth:Int = 10,
    logger:Option[Logger]=None,
    visited:mutable.Set[Any] = mutable.Set.empty
  )(implicit ev:ClassTag[X]):Set[X] = collect(x, visitFunc, depth, logger, visited)

  def collectDown[X](
    x:Any, 
    visitFunc:Any => Iterable[Any]=visitDown, 
    depth:Int = 10,
    logger:Option[Logger]=None,
    visited:mutable.Set[Any] = mutable.Set.empty
  )(implicit ev:ClassTag[X]):Set[X] = collect(x, visitFunc, depth, logger, visited)

  def collectIn[X](
    x:Any, 
    visitFunc:Any => Iterable[Any]=visitIn, 
    depth:Int = 15,
    logger:Option[Logger]=None,
    visited:mutable.Set[Any] = mutable.Set.empty
  )(implicit ev:ClassTag[X]):Set[X] = collect(x, visitFunc, depth, logger, visited)

  def collectOut[X](
    x:Any, 
    visitFunc:Any => Iterable[Any]=visitOut, 
    depth:Int = 15,
    logger:Option[Logger]=None,
    visited:mutable.Set[Any] = mutable.Set.empty
  )(implicit ev:ClassTag[X]):Set[X] = collect(x, visitFunc, depth, logger, visited)

}
