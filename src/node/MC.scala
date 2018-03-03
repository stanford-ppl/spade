package spade.node

import spade._
import spade.params._
import spade.network._

case class MC(param:MCParam, override val nios:List[NetworkBundle[_<:BundleType]])(implicit design:Design) extends Routable(nios) {
  import param._
  val wOffset = Module(FIFO[Word](wOffsetFifoParam),"wOffset")
  val rOffset = Module(FIFO[Word](rOffsetFifoParam),"rOffset")
  val wSize   = Module(FIFO[Word](wSizeFifoParam  ),"wSize"  )
  val rSize   = Module(FIFO[Word](rSizeFifoParam  ),"rSize"  )
  val sData   = Module(FIFO[Word](sDataFifoParam  ),"sData"  )
  val vData   = Module(FIFO[Word](vDataFifoParam  ),"vData"  )
}
