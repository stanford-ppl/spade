package spade
package param

trait CMeshTopParam extends TopParam {
  val numRows:Int
  val numCols:Int
  val pattern:CMeshPattern
}

import SpadeConfig._
case class StaticCMeshTopParam (
  numRows:Int=option[Int]("row"),
  numCols:Int=option[Int]("col"),
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
