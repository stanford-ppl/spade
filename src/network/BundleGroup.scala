package spade
package node

import scala.collection.mutable

case class BundleGroup(
  param:Parameter, 
  coord:Option[(Int,Int)]=None
) {
  val bundleMap = mutable.Map[ClassTag[_], GridBundle[_<:PinType]]()
  def addBundle[B<:PinType:ClassTag](bundle:GridBundle[B]) = {
    bundleMap += implicitly[ClassTag[B]] -> bundle
  }
  def bundle[B<:PinType:ClassTag] = bundleMap(implicitly[ClassTag[B]]).asInstanceOf[GridBundle[B]]
  def bundles = bundleMap.values.toList
}
