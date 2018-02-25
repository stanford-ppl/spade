package spade.config

import spade.node._
import spade.util.typealias._

import prism.collection.immutable._

import scala.reflect._
import scala.language.existentials

import SpadeMap._

trait SpadeMapLike { self:Product =>
  type S
  val fimap:FIMap
  val cfmap:CFMap

  def setFI(k:FIMap.K, v:FIMap.V):S = set[FIMap.K, FIMap.V, FIMap](k,v)

  def set[K,V,M<:MapLike[K,V,_,M]:ClassTag](k:K, v:V):S = {
    val args = productIterator.toList.map{
      case map:M => map + (k -> v) 
      case map => map
    }
    val constructor = this.getClass.getConstructors()(0) 
    constructor.newInstance(args.map(_.asInstanceOf[Object]):_*).asInstanceOf[S]
  }
}
case class SpadeMap (
  fimap:FIMap,
  cfmap:CFMap
) extends SpadeMapLike { type S = SpadeMap }
object SpadeMap {
  def empty:SpadeMap = SpadeMap(FIMap.empty, CFMap.empty) 
}

