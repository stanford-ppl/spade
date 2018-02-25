package spade.config

import spade.node._
import spade.util.typealias._

import prism.collection.immutable._
import scala.language.existentials

import CFMap._
case class CFMap(map:Map[K,V]) extends OneToOneMapLike[K,V,CFMap] {
  override def apply(n:Configurable) = n.toConfig(map(n))
  override def get(n:Configurable) = map.get(n).map{ c => n.toConfig(c) }
  def isMapped(n:K) = map.contains(n)
}

object CFMap {
  type K = Configurable
  type V = Configuration
  def empty = CFMap(Map.empty)

}
