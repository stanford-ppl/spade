package spade.params

import spade.node._
import prism.node._

case class MeshTopParam (
  numRows:Int=2,
  numCols:Int=2,
  centrolPattern:GridCentrolPattern=Checkerboard(),
  fringePattern:GridFringePattern=MCOnly(),
  val networkParams:List[MeshNetworkParam[_<:BundleType]] = List(
    MeshControlNetworkParam(),
    MeshScalarNetworkParam(),
    MeshVectorNetworkParam()
  )
) extends TopParam
