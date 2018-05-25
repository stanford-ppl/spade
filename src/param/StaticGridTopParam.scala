package spade
package param

case class StaticGridTopParam (
  numRows:Int=2,
  numCols:Int=2,
  switchParam:SwitchParam=SwitchParam(),
  centrolPattern:GridCentrolPattern=Checkerboard(),
  fringePattern:GridFringePattern=MCOnly(),
  networkParams:List[StaticGridNetworkParam[_<:PinType]] = List(
    StaticGridControlNetworkParam(),
    StaticGridScalarNetworkParam(),
    StaticGridVectorNetworkParam()
  )
) extends GridTopParam {
  //val busWithReady = false
  val busWithReady = true
}

