package spade.network

import spade._
import spade.node._
import spade.params._

import prism._
import scala.collection.mutable._

case class BundleGroup(param:Parameter, nios:ListBuffer[GridBundle[_<:BundleType]]=ListBuffer.empty, coord:Option[(Int,Int)]=None)(implicit top:MeshTop) {
  top.bundles += this
}
