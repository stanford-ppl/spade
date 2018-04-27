package spade

package object node extends spade.util.PrismAlias {
  type Spade = spade.Spade
  type SpadePass = spade.pass.SpadePass
  type SpadeWorld = spade.pass.SpadeWorld
  type SpadeTraversal = spade.pass.SpadeTraversal
  type SpadeMapLike = spade.config.SpadeMapLike
  type SpadeMetadata = spade.util.SpadeMetadata

  def indexing[T<:SpadeNode](ns:List[T])(implicit design:SpadeDesign):List[T] = {
    import design.spademeta._
    ns.zipWithIndex.foreach { case (n, i) =>
      indexOf(n) = i
    }
    ns
  }

  def naming[T<:SpadeNode](n:T, s:String)(implicit design:SpadeDesign) = {
    import design.spademeta._
    nameOf(n) = s
    n
  }

  def bctOf(x:Any):ClassTag[_] = x match {
    case x:DirectedEdge[_,_] => x.bct
    case x:Pin[_] => x.bct
    case x:Bundle[_] => x.bct
    case x:StaticMeshNetwork[_] => x.bct
    case x:StaticMeshNetworkParam[_] => x.bct
    case x:DynamicMeshNetwork[_] => x.bct
    case x:DynamicMeshNetworkParam[_] => x.bct
    case x:FIFO[_] => x.bct
    case x => throw PIRException(s"don't have ClassTag[_<:PinType] for $x")
  }

  def isInput(port:Pin[_]) = port match {
    case port:Input[_] => true
    case port:Output[_] => false
  }

  def isOutput(port:Port[_]) = port match {
    case port:Input[_] => false
    case port:Output[_] => true
  }

  def is[B<:PinType:ClassTag](x:ClassTag[_<:PinType]) = implicitly[ClassTag[B]] == x
  def isBit(x:ClassTag[_<:PinType]) = is[Bit](x)
  def isWord(x:ClassTag[_<:PinType]) = is[Word](x)
  def isVector(x:ClassTag[_<:PinType]) = is[Vector](x)
  def as[A[_<:PinType], B<:PinType:ClassTag](x:A[_])(implicit f:A[_] => ClassTag[_<:PinType]) = {
    if (is[B](f(x))) Some(x.asInstanceOf[A[B]]) else None
  }
  implicit def DirectedEdgeToBct(x:DirectedEdge[_,_]):ClassTag[_<:PinType] = x.bct.asInstanceOf[ClassTag[_<:PinType]]
  implicit def PinToBct(x:Pin[_]):ClassTag[_<:PinType] = x.bct.asInstanceOf[ClassTag[_<:PinType]]
  implicit def BundleToBct(x:Bundle[_]):ClassTag[_<:PinType] = x.bct.asInstanceOf[ClassTag[_<:PinType]]
  implicit def FIFOToBct(x:FIFO[_]):ClassTag[_<:PinType] = x.bct.asInstanceOf[ClassTag[_<:PinType]]
  implicit def DynamicMeshNetworkToBct(x:DynamicMeshNetwork[_]):ClassTag[_<:PinType] = x.bct.asInstanceOf[ClassTag[_<:PinType]]
  implicit def StaticMeshNetworkToBct(x:StaticMeshNetwork[_]):ClassTag[_<:PinType] = x.bct.asInstanceOf[ClassTag[_<:PinType]]

  def bundleOf[B<:PinType:ClassTag:TypeTag](x:SpadeNode) = {
    x.collectDown[Bundle[B]]().headOption
  }
  def cbundleOf(x:SpadeNode) = bundleOf[Bit](x)
  def sbundleOf(x:SpadeNode) = bundleOf[Word](x)
  def vbundleOf(x:SpadeNode) = bundleOf[Vector](x)

  def isMesh(n:Top) = n match {
    case n:MeshTop => true
    case _ => false
  }

  def isDynamic(n:Any) = n match {
    case n:DynamicMeshTop => true
    case n:DynamicMeshTopParam => true
    case n:DynamicMeshNetwork[_] => true
    case n:DynamicMeshNetworkParam[_] => true
    case n => false
  }

  def isStatic(n:Any) = n match {
    case n:StaticMeshTop => true
    case n:StaticMeshTopParam => true
    case n:StaticMeshNetwork[_] => true
    case n:StaticMeshNetworkParam[_] => true
    case n => false
  }

  def cuOf(n:SpadeNode) = n.collectUp[CU]().headOption

  def routableOf(n:SpadeNode) = n.collectUp[Routable]().headOption
}
