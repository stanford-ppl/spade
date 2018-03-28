package spade.network

import spade._
import spade.node._
import spade.params._

import prism._
import scala.collection.mutable._

case class BundleSet(param:Parameter, nios:ListBuffer[GridBundle[_<:PinType]]=ListBuffer.empty, coord:Option[(Int,Int)]=None)(implicit top:MeshTop) {
  top.bundles += this
}
