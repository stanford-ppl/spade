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
    logger:Option[Logger]=None,
    visited:mutable.Set[Any] = mutable.Set.empty
  )(implicit ev:ClassTag[X]):Set[X] = {
    def f(xx:Any):Set[X] = {
      visited += x
      collect[X](xx, visitFunc, logger, visited)
    }
    logger.foreach { _.emitBSln(s"collect($x) ${visited.contains(x)}") }
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
    logger:Option[Logger]=None,
    visited:mutable.Set[Any] = mutable.Set.empty
  )(implicit ev:ClassTag[X]):Set[X] = collect(x, visitFunc, logger, visited)

  def collectDown[X](
    x:Any, 
    visitFunc:Any => Iterable[Any]=visitDown, 
    logger:Option[Logger]=None,
    visited:mutable.Set[Any] = mutable.Set.empty
  )(implicit ev:ClassTag[X]):Set[X] = collect(x, visitFunc, logger, visited)

  def collectIn[X](
    x:Any, 
    visitFunc:Any => Iterable[Any]=visitIn, 
    logger:Option[Logger]=None,
    visited:mutable.Set[Any] = mutable.Set.empty
  )(implicit ev:ClassTag[X]):Set[X] = collect(x, visitFunc, logger, visited)

  def collectOut[X](
    x:Any, 
    visitFunc:Any => Iterable[Any]=visitOut, 
    logger:Option[Logger]=None,
    visited:mutable.Set[Any] = mutable.Set.empty
  )(implicit ev:ClassTag[X]):Set[X] = collect(x, visitFunc, logger, visited)

}
