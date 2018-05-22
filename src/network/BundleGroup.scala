package spade
package node

import scala.collection.mutable

case class BundleGroup(param:Parameter, bundles:ListBuffer[GridBundle[_<:PinType]]=ListBuffer.empty, coord:Option[(Int,Int)]=None)
