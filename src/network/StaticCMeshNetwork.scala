package spade
package node

import scala.collection.mutable

class StaticCMeshNetwork[B<:PinType](
  param:StaticCMeshNetworkParam[B], 
  top:StaticCMeshTop
)(implicit design:SpadeDesign) extends Network[B](param, top){
  import param._
  import top._

  def connect(a:BundleGroup, b:BundleGroup):Unit = {
    a.connect[B](b, channelWidth("src"->tpOf(a), "dst"->tpOf(b)))
    b.connect[B](a, channelWidth("src"->tpOf(b), "dst"->tpOf(a)))
  }

  cuArray.zipWithIndex.foreach { case (col, i) =>
    col.zipWithIndex.foreach { case (cluster, j) =>
      val sb = sbArray(i)(j)
      /* ---- CU to SB connections ----*/
      cluster.foreach { col => col.foreach { cu => connect(cu, sb) } }
      /* ---- CU to CU connections ----*/
      connect(cluster(0)(0), cluster(0)(1))
      connect(cluster(0)(1), cluster(1)(1))
      connect(cluster(1)(1), cluster(1)(0))
      connect(cluster(1)(0), cluster(0)(0))
    }
  }

  for (y <- 0 until numRows) {
    for (x <- 0 until numCols) {

      /* ---- SB to SB connections ----*/
      if (x!=numCols-1) connect(sbArray(x)(y), sbArray(x+1)(y)) // (Horizontal)
      if (y!=numRows-1) connect(sbArray(x)(y), sbArray(x)(y+1)) // (Vertical)

      /* ---- Top to SB connections ----*/
      if (y==numRows-1) connect(argFringe, sbArray(x)(y)) // Top Switches
      //if (y==0) connect(argFringe, sbArray(x)(y)) // Bottom Switches

    }
  }

}
