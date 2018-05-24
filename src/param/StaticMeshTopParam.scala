package spade
package param

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

