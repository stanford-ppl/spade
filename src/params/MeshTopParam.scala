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
  switchParam:SwitchParam=SwitchParam(),
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
  routerParam:RouterParam=RouterParam(),
  centrolPattern:GridCentrolPattern=Checkerboard(),
  fringePattern:GridFringePattern=MCOnly(),
  networkParams:List[DynamicMeshNetworkParam[_<:PinType]] = List(
    DynamicMeshControlNetworkParam(),
    DynamicMeshScalarNetworkParam(),
    DynamicMeshVectorNetworkParam()
  )
) extends MeshTopParam {
  val busWithReady = true
  val fringeNumCols = fringePattern match {
    case _:MCOnly => 1
    case _:MC_DramAG => 2
  }
  // Oneside
  val numTotalRows = numRows + 1 // one row for arg fringe
  val numTotalCols = numCols+fringeNumCols*2
}
