package spade
package node

trait CMeshTopParam extends TopParam {
  val numRows:Int
  val numCols:Int
  val pattern:CMeshPattern
}

case class StaticCMeshTopParam (
  numRows:Int=2,
  numCols:Int=2,
  switchParam:SwitchParam=SwitchParam(),
  pattern:CMeshPattern=CMeshCheckerboard(),
  networkParams:List[StaticCMeshNetworkParam[_<:PinType]] = List(
    StaticCMeshControlNetworkParam(),
    StaticCMeshScalarNetworkParam(),
    StaticCMeshVectorNetworkParam()
  )
) extends CMeshTopParam {
  val busWithReady = true
}
