package spade
package node

trait MeshTopParam extends TopParam {
  val numRows:Int
  val numCols:Int
  val centrolPattern:GridCentrolPattern
  val fringePattern:GridFringePattern
}

case class StaticMeshTopParam (
  numRows:Int=2,
  numCols:Int=2,
  centrolPattern:GridCentrolPattern=Checkerboard(),
  fringePattern:GridFringePattern=MCOnly(),
  networkParams:List[StaticMeshNetworkParam[_<:PinType]] = List(
    StaticMeshControlNetworkParam(),
    StaticMeshScalarNetworkParam(),
    StaticMeshVectorNetworkParam()
  )
) extends MeshTopParam {
  //val busWithReady = false
  val busWithReady = true
}

case class DynamicMeshTopParam (
  numRows:Int=2,
  numCols:Int=2,
  centrolPattern:GridCentrolPattern=Checkerboard(),
  fringePattern:GridFringePattern=MCOnly(),
  networkParams:List[DynamicMeshNetworkParam[_<:PinType]] = List(
    DynamicMeshControlNetworkParam(),
    DynamicMeshScalarNetworkParam(),
    DynamicMeshVectorNetworkParam()
  )
) extends MeshTopParam {
  val busWithReady = true
}
