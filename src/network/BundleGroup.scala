package spade.node

case class BundleGroup(param:Parameter, nios:ListBuffer[GridBundle[_<:PinType]]=ListBuffer.empty, coord:Option[(Int,Int)]=None)(implicit top:MeshTop) {
  top.bundles += this
}
