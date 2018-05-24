package spade
package param

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
