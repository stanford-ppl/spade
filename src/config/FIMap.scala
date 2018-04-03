package spade.config

import spade.node._

import prism.collection.immutable._

import FIMap._
case class FIMap(fmap:OneToOneMap[K,V], bmap:OneToManyMap[V,K]) extends BiManyToOneMapLike[K,V,FIMap] {
  def apply(v:V):KK = bmap(v)
  def get(v:V):Option[KK] = bmap.get(v)
  def contains(v:V) = bmap.contains(v)
}

object FIMap {
  type K = Input[_<:PinType] 
  type V = Output[_<:PinType]
  def empty = FIMap(OneToOneMap.empty, OneToManyMap.empty)
}
