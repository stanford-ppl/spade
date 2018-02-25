package spade.config

import spade.node._

import prism.collection.immutable._
import scala.language.existentials

import FIMap._
case class FIMap(fmap:OneToOneMap[K,V], bmap:OneToManyMap[V,K]) extends BiManyToOneMapLike[K,V,FIMap] {
  def get(k:GlobalInput[_<:PortType, _<:Module]) = { map.get(k).asInstanceOf[Option[GlobalOutput[_<:PortType, _<:Module]]] }
  def apply(v:V):KK = bmap(v)
  def get(v:V):Option[KK] = bmap.get(v)
  def contains(v:V) = bmap.contains(v)
}

object FIMap {
  type K = Input[_<:PortType, _<:Module] 
  type V = Output[_<:PortType, _<:Module]
  def empty = FIMap(OneToOneMap.empty, OneToManyMap.empty)
}
