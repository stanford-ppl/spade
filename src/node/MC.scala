package spade
package node

case class MC(param:MCParam, override val nios:List[Bundle[_<:PinType]])(implicit design:SpadeDesign) extends Routable(nios) {
  import param._
  val wOffset = Module(FIFO[Word](wOffsetFifoParam),"wOffset")
  val rOffset = Module(FIFO[Word](rOffsetFifoParam),"rOffset")
  val wSize   = Module(FIFO[Word](wSizeFifoParam  ),"wSize"  )
  val rSize   = Module(FIFO[Word](rSizeFifoParam  ),"rSize"  )
  val sData   = Module(FIFO[Word](sDataFifoParam  ),"sData"  )
  val vData   = Module(FIFO[Word](vDataFifoParam  ),"vData"  )
}
