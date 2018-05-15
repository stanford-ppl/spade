package spade
package node

import scala.collection.mutable

class StaticMeshNetwork[B<:PinType](param:StaticMeshNetworkParam[B], top:StaticMeshTop)(implicit design:SpadeDesign) {
  implicit val bct = param.bct
  import param._
  import top._

  val bundleOf = mutable.Map[BundleGroup, GridBundle[B]]()

  bundleGroups.foreach { node => 
    val bundle = GridBundle[B]()
    node.nios += bundle
    bundleOf(node) = bundle
  }

  def tpOf(node:BundleGroup) = node.param match {
    case param:PCUParam => "pcu"
    case param:PMUParam => "pmu"
    case param:SCUParam => "scu"
    case param:SwitchParam => "sb"
    case param:ArgFringeParam => "arg"
    case param:MCParam => "mc"
  }

  def connect(src:BundleGroup, outDir:String, dst:BundleGroup, inDir:String)(implicit design:SpadeDesign):Unit = {
    val cw = channelWidth("src"->tpOf(src), "dst"->tpOf(dst), "srcDir"->inDir, "dstDir"->outDir)
    val outs = bundleOf(src).addOutAt(outDir, cw)
    val ins = bundleOf(dst).addInAt(inDir, cw)
    outs.zip(ins).foreach { case (o, i) => i <== o }
  }

  /** ----- Central Array Connection ----- **/
  for (y <- 0 until numRows) {
    for (x <- 0 until numCols) {
      /* ----- CU to CU Connection ----- */
      // CU to CU (Horizontal)
      if (x!=numCols-1) {
        // W -> E
        connect(cuArray(x)(y), "E", cuArray(x+1)(y), "W")
        // E -> W
        connect(cuArray(x+1)(y), "W", cuArray(x)(y), "E")
      }
      //// CU to CU (Vertical)
      if (y!=numRows-1) {
        // S -> N
        connect(cuArray(x)(y), "N", cuArray(x)(y+1), "S")
        // N -> S 
        connect(cuArray(x)(y+1), "S", cuArray(x)(y), "N")
      }


      /* ----- CU to SB Connection ----- */
      // NW (top left)
      connect(cuArray(x)(y), "NW", sbArray(x)(y+1), "SE")
      // NE (top right)
      connect(cuArray(x)(y), "NE", sbArray(x+1)(y+1), "SW")
      // SW (bottom left)
      connect(cuArray(x)(y), "SW", sbArray(x)(y), "NE")
      // SE (bottom right)
      connect(cuArray(x)(y), "SE", sbArray(x+1)(y), "NW")

      // SB to CU
      // NW (top left)
      connect(sbArray(x)(y+1), "SE", cuArray(x)(y), "NW")
      // NE (top right)
      connect(sbArray(x+1)(y+1), "SW", cuArray(x)(y), "NE")
      // SW (bottom left)
      connect(sbArray(x)(y), "NE", cuArray(x)(y), "SW")
      // SE (bottom right)
      connect(sbArray(x+1)(y), "NW", cuArray(x)(y), "SE")
    }
  }

  for (y <- 0 until numRows+1) {
    for (x <- 0 until numCols+1) {

      /* ---- SB to SB connections ----*/
      // SB to SB (Horizontal)
      if (x!=numCols) {
        // W -> E 
        connect(sbArray(x)(y), "E", sbArray(x+1)(y), "W")
        // E -> W
        connect(sbArray(x+1)(y), "W", sbArray(x)(y), "E")
      }
      // SB to SB (Vertical)
      if (y!=numRows) {
        // S -> N
        connect(sbArray(x)(y), "N", sbArray(x)(y+1), "S")
        // N -> S 
        connect(sbArray(x)(y+1), "S", sbArray(x)(y), "N")
      }

      // Top to SB
      // Top Switches
      if (y==numRows) {
        // S -> N
        connect(sbArray(x)(y), "N", argFringe, "S") // bottom up 
        // N -> S
        connect(argFringe, "S", sbArray(x)(y), "N") // top down
      }
      // Bottom Switches
      if (y==0) {
        // N -> S
        connect(sbArray(x)(y), "S", argFringe, "N") // top down 
        // S -> N
        connect(argFringe, "N", sbArray(x)(y), "S") // bottom up
      }


      ///* ---- OCU and SB connection ----*/
      //// OCU to SB 
      //connect(ocuArray(x)(y), "W", sbArray(x)(y), "E")

      //// SB to OCU
      //connect(sbArray(x)(y), "E", ocuArray(x)(y), "W")
    }
  }

  /** ----- Fringe Connection ----- **/
  for (y <- 0 until mcArray.headOption.map(_.size).getOrElse(0)) { //cols
    for (x <- 0 until mcArray.size) { //rows

      ///* ---- DramAddrGen and SwitchBox connection ---- */
      //if (x==0) {
        //// DAG to SB (W -> E) (left side)
        //connect(dramAGs(x)(y), "E", sbArray(0)(y), "W")
        //// SB to DAG (E -> W) (left side)
        //connect(sbArray(0)(y), "W", dramAGs(x)(y), "E")
      //} else {
        //// DAG to SB (E -> W) (right side)
        //connect(dramAGs(x)(y), "W", sbArray(numCols)(y), "E")
        //// SB to DAG (W -> E) (right side)
        //connect(sbArray(numCols)(y), "E", dramAGs(x)(y), "W")
      //}

      ///* ---- SramAddrGen and SwitchBox connection ---- */
      //if (x==0) {
        //// SAG to SB (W -> E) (left side)
        //connect(sramAGs(x)(y), "E", sbArray(0)(y), "W")
        //// SB to SAG (E -> W) (left side)
        //connect(sbArray(0)(y), "W", sramAGs(x)(y), "E")
      //} else {
        //// SAG to SB (E -> W) (right side)
        //connect(sramAGs(x)(y), "W", sbArray(numCols)(y), "E")
        //// SB to SAG (W -> E) (right side)
        //connect(sbArray(numCols)(y), "E", sramAGs(x)(y), "W")
      //}

      ///* ---- MC and SwitchBox connection ---- */
      if (x==0) {
        // MC to SB (W -> E) (left side)
        connect(mcArray(x)(y), "E", sbArray(0)(y), "W")
        // SB to MC (E -> W) (left side)
        connect(sbArray(0)(y), "W", mcArray(x)(y), "E")
      } else {
        // MC to SB (E -> W) (right side)
        connect(mcArray(x)(y), "W", sbArray(numCols)(y), "E")
        // SB to MC (W -> E) (right side)
        connect(sbArray(numCols)(y), "E", mcArray(x)(y), "W")
      }

      ///* ---- MC and DramAddrGen connection ---- */
      //val pos = if (x==0) "left" else "right"
      //// MC to DAG (S -> N)
      //connect(mcArray(x)(y), "N", dramAGs(x)(y), "S", pos)
      //// DAG to MC (N -> S)
      //connect(dramAGs(x)(y), "S", mcArray(x)(y), "N", pos)
    }
  }

  bundleOf.values.foreach { bundle =>
    indexing(bundle.inputs)
    indexing(bundle.outputs)
  }
}
