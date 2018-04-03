package spade.config

import spade.node._

import prism.collection.immutable._

import SpadeMap._

trait SpadeMapLike { self:Product =>
  type S <: SpadeMapLike
  val fimap:FIMap
  val cfmap:ConfigMap

  def set[M<:MapLike[_,_,_,M]:ClassTag](k:Any, v:Any):S = {
    val args = productIterator.toList.map{
      case map:M => map + (k.asInstanceOf[map.K] -> v.asInstanceOf[map.V]) 
      case map => map
    }
    val constructor = this.getClass.getConstructors()(0) 
    constructor.newInstance(args.map(_.asInstanceOf[Object]):_*).asInstanceOf[S]
  }
  def get[F:ClassTag]:F = {
    productIterator.toList.collect { case f:F => f }.head
  }
  def set[F:ClassTag](newField:F):S = {
    val args = productIterator.toList.map {
      case field:F => newField
      case field => field
    }
    val constructor = this.getClass.getConstructors()(0) 
    constructor.newInstance(args.map(_.asInstanceOf[Object]):_*).asInstanceOf[S]
  }
  def map[F:ClassTag](lambda: F => F):S = {
    val args = productIterator.toSeq.map {
      case field:F => lambda(field)
      case field => field
    }
    val constructor = this.getClass.getConstructors()(0) 
    constructor.newInstance(args.map(_.asInstanceOf[Object]):_*).asInstanceOf[S]
  }
}
case class SpadeMap (
  fimap:FIMap,
  cfmap:ConfigMap
) extends SpadeMapLike { type S = SpadeMap }
object SpadeMap {
  def empty:SpadeMap = SpadeMap(FIMap.empty, ConfigMap.empty) 
}

