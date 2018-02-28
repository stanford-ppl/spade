package spade.params

import prism.enums._
import prism.node._

trait DesignParam extends Parameter

case class MeshDesignParam(
  numRows:Int=2,
  numCols:Int=2,
  pattern:GridPattern=Checkerboard(),
  argFringeParam:ArgFringeParam=ArgFringeParam()
)(
  val networkParams:List[MeshNetworkParam[_<:BundleType]] = List(
    MeshControlNetworkParam(numRows,numCols,pattern,argFringeParam),
    MeshScalarNetworkParam(numRows,numCols,pattern,argFringeParam),
    MeshVectorNetworkParam(numRows,numCols,pattern,argFringeParam)
  )
) extends DesignParam
