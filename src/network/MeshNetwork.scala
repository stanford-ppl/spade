package spade.network

import spade._
import spade.node._
import spade.params._
import spade.util._

import prism._
import prism.collection.mutable.Table

import scala.language.reflectiveCalls

import scala.collection.mutable

class MeshNetwork[B<:BundleType](param:MeshNetworkParam[B])(implicit design:Design) {
  implicit val bct = param.bct
  import param._

  val bundles = mutable.ListBuffer[GridBundle[B]]()
  case class Node(tp:String, bundle:GridBundle[B]=GridBundle[B]()) {
    bundles += bundle
  }

  val arg = Node("arg") 
  val argBundle = arg.bundle
  val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    val name = pattern.cuAt(i,j) match {
      case param:PCUParam => "pcu"
      case param:PMUParam => "pmu"
      case param:SCUParam => "scu"
    }
    Node(name)
  }
  val cuBundles = cuArray.map(_.map(_.bundle))
  val sbArray = List.tabulate(numCols + 1, numRows + 1) { case (i,j) => Node("sb") }
  val switchBundle = sbArray.map(_.map(_.bundle))

  def connect(out:Node, outDir:String, in:Node, inDir:String, pos:String)(implicit design:Design):Unit = {
    val cw = channelWidth("pos"->pos, "src"->out.tp, "dst"->in.tp, "srcDir"->inDir, "dstDir"->outDir)
    val key = Seq("pos"->pos, "src"->out.tp, "dst"->in.tp, "srcDir"->inDir, "dstDir"->outDir)
    (out.tp, in.tp) match {
      case ("arg", _) =>
        val outs = out.bundle.outputs
        outs.foreach { argIn =>
          val ins = in.bundle.addInAt(inDir, cw)
          ins.foreach { _ <== argIn }
        }
      case (_, "arg") =>
        val outs = out.bundle.addOutAt(outDir, cw)
        val ins = in.bundle.inputs
        ins.foreach { _ <== outs }
      case (_, _) =>
        val outs = out.bundle.addOutAt(outDir, cw)
        val ins = in.bundle.addInAt(inDir, cw)
        outs.zip(ins).foreach { case (o, i) => i <== o }
    }
  }


  if (is[Word](this)) {
    argBundle.addInAt("S", argFringeParam.numArgOuts)
    argBundle.addOutAt("S", argFringeParam.numArgIns)
  } else if (is[Bit](this)) {
    argBundle.addInAt("S", 1) //status
    argBundle.addOutAt("S", 1) //command
  }

  /** ----- Central Array Connection ----- **/
  for (y <- 0 until numRows) {
    for (x <- 0 until numCols) {
      /* ----- CU to CU Connection ----- */
      // CU to CU (Horizontal)
      if (x!=numCols-1) {
        // W -> E
        connect(cuArray(x)(y), "E", cuArray(x+1)(y), "W", "center")
        // E -> W
        connect(cuArray(x+1)(y), "W", cuArray(x)(y), "E", "center")
      }
      //// CU to CU (Vertical)
      if (y!=numRows-1) {
        // S -> N
        connect(cuArray(x)(y), "N", cuArray(x)(y+1), "S", "center")
        // N -> S 
        connect(cuArray(x)(y+1), "S", cuArray(x)(y), "N", "center")
      }


      /* ----- CU to SB Connection ----- */
      // NW (top left)
      connect(cuArray(x)(y), "NW", sbArray(x)(y+1), "SE", "center")
      // NE (top right)
      connect(cuArray(x)(y), "NE", sbArray(x+1)(y+1), "SW", "center")
      // SW (bottom left)
      connect(cuArray(x)(y), "SW", sbArray(x)(y), "NE", "center")
      // SE (bottom right)
      connect(cuArray(x)(y), "SE", sbArray(x+1)(y), "NW", "center")

      // SB to CU
      // NW (top left)
      connect(sbArray(x)(y+1), "SE", cuArray(x)(y), "NW", "center")
      // NE (top right)
      connect(sbArray(x+1)(y+1), "SW", cuArray(x)(y), "NE", "center")
      // SW (bottom left)
      connect(sbArray(x)(y), "NE", cuArray(x)(y), "SW", "center")
      // SE (bottom right)
      connect(sbArray(x+1)(y), "NW", cuArray(x)(y), "SE", "center")
    }
  }

  for (y <- 0 until numRows+1) {
    for (x <- 0 until numCols+1) {

      /* ---- SB to SB connections ----*/
      // SB to SB (Horizontal)
      if (x!=numCols) {
        // W -> E 
        connect(sbArray(x)(y), "E", sbArray(x+1)(y), "W", "center")
        // E -> W
        connect(sbArray(x+1)(y), "W", sbArray(x)(y), "E", "center")
      }
      // SB to SB (Vertical)
      if (y!=numRows) {
        // S -> N
        connect(sbArray(x)(y), "N", sbArray(x)(y+1), "S", "center")
        // N -> S 
        connect(sbArray(x)(y+1), "S", sbArray(x)(y), "N", "center")
      }

      // Top to SB
      // Top Switches
      if (y==numRows) {
        // S -> N
        connect(sbArray(x)(y), "N", arg, "S", "top") // bottom up 
        // N -> S
        connect(arg, "S", sbArray(x)(y), "N", "top") // top down
      }
      // Bottom Switches
      if (y==0) {
        // N -> S
        connect(sbArray(x)(y), "S", arg, "N", "bottom") // top down 
        // S -> N
        connect(arg, "N", sbArray(x)(y), "S", "bottom") // bottom up
      }


      ///* ---- OCU and SB connection ----*/
      //// OCU to SB 
      //connect(ocuArray(x)(y), "W", sbArray(x)(y), "E", "center")

      //// SB to OCU
      //connect(sbArray(x)(y), "E", ocuArray(x)(y), "W", "center")
    }
  }

  /** ----- Fringe Connection ----- **/
  //for (y <- 0 until mcArray.headOption.map(_.size).getOrElse(0)) { //cols
    //for (x <- 0 until mcArray.size) { //rows

      ///* ---- DramAddrGen and SwitchBox connection ---- */
      //if (x==0) {
        //// DAG to SB (W -> E) (left side)
        //connect(dramAGs(x)(y), "E", sbArray(0)(y), "W", "left")
        //// SB to DAG (E -> W) (left side)
        //connect(sbArray(0)(y), "W", dramAGs(x)(y), "E", "left")
      //} else {
        //// DAG to SB (E -> W) (right side)
        //connect(dramAGs(x)(y), "W", sbArray(numCols)(y), "E", "right")
        //// SB to DAG (W -> E) (right side)
        //connect(sbArray(numCols)(y), "E", dramAGs(x)(y), "W", "right")
      //}

      ///* ---- SramAddrGen and SwitchBox connection ---- */
      //if (x==0) {
        //// SAG to SB (W -> E) (left side)
        //connect(sramAGs(x)(y), "E", sbArray(0)(y), "W", "left")
        //// SB to SAG (E -> W) (left side)
        //connect(sbArray(0)(y), "W", sramAGs(x)(y), "E", "left")
      //} else {
        //// SAG to SB (E -> W) (right side)
        //connect(sramAGs(x)(y), "W", sbArray(numCols)(y), "E", "right")
        //// SB to SAG (W -> E) (right side)
        //connect(sbArray(numCols)(y), "E", sramAGs(x)(y), "W", "right")
      //}

      ///* ---- MC and SwitchBox connection ---- */
      //if (x==0) {
        //// MC to SB (W -> E) (left side)
        //connect(mcArray(x)(y), "E", sbArray(0)(y), "W", "left")
        //// SB to MC (E -> W) (left side)
        //connect(sbArray(0)(y), "W", mcArray(x)(y), "E", "left")
      //} else {
        //// MC to SB (E -> W) (right side)
        //connect(mcArray(x)(y), "W", sbArray(numCols)(y), "E", "right")
        //// SB to MC (W -> E) (right side)
        //connect(sbArray(numCols)(y), "E", mcArray(x)(y), "W", "right")
      //}

      ///* ---- MC and DramAddrGen connection ---- */
      //val pos = if (x==0) "left" else "right"
      //// MC to DAG (S -> N)
      //connect(mcArray(x)(y), "N", dramAGs(x)(y), "S", pos)
      //// DAG to MC (N -> S)
      //connect(dramAGs(x)(y), "S", mcArray(x)(y), "N", pos)
    //}
  //}

  bundles.foreach { bundle =>
    indexing(bundle.inputs)
    indexing(bundle.outputs)
  }
}
