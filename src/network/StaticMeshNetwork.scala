package spade
package node

import scala.collection.mutable

class StaticMeshNetwork[B<:PinType](param:StaticMeshNetworkParam[B], top:StaticMeshTop)(implicit design:SpadeDesign) {
  implicit val bct = param.bct
  import param._
  import top._

  bundleGroups.foreach { node => node.addBundle(GridBundle[B]()) }

  def tpOf(node:BundleGroup) = node.param match {
    case param:PCUParam => "pcu"
    case param:PMUParam => "pmu"
    case param:SCUParam => "scu"
    case param:SwitchParam => "sb"
    case param:ArgFringeParam => "arg"
    case param:MCParam => "mc"
  }

  def connect(src:BundleGroup, dst:BundleGroup, outDir:String, inDir:String):Unit = {
    val cw = channelWidth("src"->tpOf(src), "dst"->tpOf(dst), "srcDir"->inDir, "dstDir"->outDir)
    src.connect[B](dst, cw)
  }

  def connect(src:BundleGroup, dst:BundleGroup):Unit = (src, dst) match {
    case (BundleGroup(_, Some((sx, sy))), BundleGroup(_, Some((dx, dy)))) if (sy < dy & sx == dx) => connect(src, dst, "S", "N")
    case (BundleGroup(_, Some((sx, sy))), BundleGroup(_, Some((dx, dy)))) if (sy > dy & sx == dx) => connect(src, dst, "N", "S")
    case (BundleGroup(_, Some((sx, sy))), BundleGroup(_, Some((dx, dy)))) if (sy == dy & sx < dx) => connect(src, dst, "W", "E")
    case (BundleGroup(_, Some((sx, sy))), BundleGroup(_, Some((dx, dy)))) if (sy == dy & sx > dx) => connect(src, dst, "E", "W")
    case (BundleGroup(_, Some((sx, sy))), BundleGroup(_, Some((dx, dy)))) if (sy < dy & sx < dx) => connect(src, dst, "SW", "NE")
    case (BundleGroup(_, Some((sx, sy))), BundleGroup(_, Some((dx, dy)))) if (sy > dy & sx < dx) => connect(src, dst, "NW", "SE")
    case (BundleGroup(_, Some((sx, sy))), BundleGroup(_, Some((dx, dy)))) if (sy < dy & sx > dx) => connect(src, dst, "SE", "NW")
    case (BundleGroup(_, Some((sx, sy))), BundleGroup(_, Some((dx, dy)))) if (sy > dy & sx > dx) => connect(src, dst, "NE", "SW")
  }

  /** ----- Central Array Connection ----- **/
  for (y <- 0 until numRows) {
    for (x <- 0 until numCols) {
      /* ----- CU to CU Connection ----- */
      // CU to CU (Horizontal)
      if (x!=numCols-1) {
        connect(cuArray(x)(y), cuArray(x+1)(y))
        connect(cuArray(x+1)(y), cuArray(x)(y))
      }
      // CU to CU (Vertical)
      if (y!=numRows-1) {
        connect(cuArray(x)(y), cuArray(x)(y+1))
        connect(cuArray(x)(y+1), cuArray(x)(y))
      }

      /* ----- CU to SB Connection ----- */
      connect(cuArray(x)(y), sbArray(x)(y))
      connect(cuArray(x)(y), sbArray(x)(y+1))
      connect(cuArray(x)(y), sbArray(x+1)(y))
      connect(cuArray(x)(y), sbArray(x+1)(y+1))

      /* ----- SB to CU Connection ----- */
      connect(sbArray(x)(y), cuArray(x)(y))
      connect(sbArray(x)(y+1), cuArray(x)(y))
      connect(sbArray(x+1)(y), cuArray(x)(y))
      connect(sbArray(x+1)(y+1), cuArray(x)(y))
    }
  }

  for (y <- 0 until numRows+1) {
    for (x <- 0 until numCols+1) {

      /* ---- SB to SB connections ----*/
      // SB to SB (Horizontal)
      if (x!=numCols) {
        connect(sbArray(x)(y), sbArray(x+1)(y))
        connect(sbArray(x+1)(y), sbArray(x)(y))
      }
      // SB to SB (Vertical)
      if (y!=numRows) {
        connect(sbArray(x)(y), sbArray(x)(y+1))
        connect(sbArray(x)(y+1), sbArray(x)(y))
      }

      /* ---- Top to SB connections ----*/
      if (y==numRows) { // Top Switches
        connect(argFringe, sbArray(x)(y)) // top down
        connect(sbArray(x)(y), argFringe) // bottom up 
      }
      //if (y==0) { // Bottom Switches
        //connect(argFringe, sbArray(x)(y)) // bottom up
        //connect(sbArray(x)(y), argFringe) // top down 
      //}

    }
  }

  ///** ----- Fringe Connection ----- **/
  for (y <- 0 until mcArray.headOption.map(_.size).getOrElse(0)) { //cols
    for (x <- 0 until mcArray.size) { //rows

      ///* ---- MC and SwitchBox connection ---- */
      if (x==0) {
        connect(mcArray(x)(y), sbArray(0)(y)) // MC to SB (W -> E) (left side)
        connect(sbArray(0)(y), mcArray(x)(y)) // SB to MC (E -> W) (left side)
      } else {
        connect(mcArray(x)(y), sbArray(numCols)(y)) // MC to SB (E -> W) (right side)
        connect(sbArray(numCols)(y), mcArray(x)(y)) // SB to MC (W -> E) (right side)
      }
    }
  }

  dagArray.foreach { dagArray =>
    for (y <- 0 until mcArray.headOption.map(_.size).getOrElse(0)) { //cols
      for (x <- 0 until mcArray.size) { //rows

        ///* ---- DramAddrGen and SwitchBox connection ---- */
        if (x==0) {
          connect(dagArray(x)(y), sbArray(0)(y)) // DAG to SB (W -> E) (left side)
          connect(sbArray(0)(y), dagArray(x)(y)) // SB to DAG (E -> W) (left side)
        } else {
          connect(dagArray(x)(y), sbArray(numCols)(y)) // DAG to SB (E -> W) (right side)
          connect(sbArray(numCols)(y), dagArray(x)(y)) // SB to DAG (W -> E) (right side)
        }

        ///* ---- MC and DramAddrGen connection ---- */
        connect(mcArray(x)(y), dagArray(x)(y)) // MC to DAG (S -> N)
        connect(dagArray(x)(y), mcArray(x)(y)) // DAG to MC (N -> S)
      }
    }
  }

}
