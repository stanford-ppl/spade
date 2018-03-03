package spade.params

import spade.node._
import prism.node._

case class MeshTopParam(
  numRows:Int=2,
  numCols:Int=2,
  pattern:GridPattern=Checkerboard(),
  argFringeParam:ArgFringeParam=ArgFringeParam(),
  mcParam:MCParam=MCParam(),
  switchParam:SwitchParam=SwitchParam()
)(
  val networkParams:List[MeshNetworkParam[_<:BundleType]] = List(
    MeshControlNetworkParam(numRows,numCols,pattern,argFringeParam),
    MeshScalarNetworkParam(numRows,numCols,pattern,argFringeParam),
    MeshVectorNetworkParam(numRows,numCols,pattern,argFringeParam)
  )
) extends TopParam {
  override def fields = super.fields :+ networkParams
}

