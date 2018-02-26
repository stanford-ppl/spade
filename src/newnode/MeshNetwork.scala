package spade.newnode

import spade._
import prism.node._
import pirc.enums._
import pirc.collection.mutable.Table

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable._

case class MeshTopParam (
  numRows:Int,
  numCols:Int,
  pattern:GridPattern,
  argFringeParam:ArgFringeParam,
  networkParams:List[MeshNetworkParam[_<:BundleType]]
) extends TopParam
case class MeshTop(param:MeshTopParam)(implicit design:Spade) extends Top(param) {
  import param._
  val networks = networkParams.map { param => new MeshNetwork(param) }

  val argFringe = {
    val bundles = networks.map(_.argBundle._1)
    Module(ArgFringe(argFringeParam, bundles))
  }

  val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    val param = pattern.cuAt(i,j)
    val bundles = networks.map(_.cuBundles(i)(j)._1)
    param match {
      case param:PCUParam => Module(PCU(param, bundles))
      case param:PMUParam => Module(PMU(param, bundles))
      case param:SCUParam => Module(SCU(param, bundles))
    }
  }

  val switchArray = List.tabulate(numCols + 1, numRows + 1) { case (i,j) => 
    val bundles = networks.map(_.switchBundles(i)(j)._1)
    Module(SwitchBox(bundles))
  }
}

case class MeshNetworkParam[B<:BundleType:ClassTag] (
  numRows:Int,
  numCols:Int,
  pattern:GridPattern,
  argFringeParam:ArgFringeParam,
  channelWidth:Table[String, String, Int] // Column, Row, Value
)
class MeshNetwork[B<:BundleType:ClassTag](param:MeshNetworkParam[B])(implicit design:Spade) {
  import param._

  type Node = (GridBundle[B], String)

  val argBundle = (GridBundle[B](), "arg")
  val cuBundles = List.tabulate(numCols, numRows) { case (i,j) => 
    val name = pattern.cuAt(i,j) match {
      case param:PCUParam => "pcu"
      case param:PMUParam => "pmu"
      case param:SCUParam => "scu"
    }
    (GridBundle[B](), name)
  }
  val switchBundles = List.tabulate(numCols + 1, numRows + 1) { case (i,j) => 
    (GridBundle[B](), "switch")
  }

  def connect(outNode:Node, outDir:String, inNode:Node, inDir:String, pos:String)(implicit design:Spade):Unit = {
    val (out, outType) = outNode
    val (in, inType) = inNode
    val cw = channelWidth("pos"->pos, "src"->outType, "dst"->inType, "srcDir"->inDir, "dstDir"->outDir)
    (out, in) match {
      case (out, in) if outType=="arg" =>
        val outs = out.outputs
        outs.foreach { argIn =>
          val ins = in.addInAt(inDir, cw)
          ins.foreach { _ <== argIn }
        }
      case (out, in) if inType=="arg" =>
        val outs = out.addOutAt(outDir, cw)
        val ins = in.inputs
        ins.foreach { _ <== outs }
      case (out, in) =>
        val outs = out.addOutAt(outDir, cw)
        val ins = in.addInAt(inDir, cw)
        outs.zip(ins).foreach { case (o, i) => i <== o }
    }
  }


  if (isScalar[B]) {
    argBundle._1.addInAt("S", argFringeParam.numArgOuts)
    argBundle._1.addOutAt("S", argFringeParam.numArgIns)
  }

  /** ----- Central Array Connection ----- **/
  for (y <- 0 until numRows) {
    for (x <- 0 until numCols) {
      /* ----- CU to CU Connection ----- */
      // CU to CU (Horizontal)
      if (x!=numCols-1) {
        // W -> E
        connect(cuBundles(x)(y), "E", cuBundles(x+1)(y), "W", "center")
        // E -> W
        connect(cuBundles(x+1)(y), "W", cuBundles(x)(y), "E", "center")
      }
      //// CU to CU (Vertical)
      if (y!=numRows-1) {
        // S -> N
        connect(cuBundles(x)(y), "N", cuBundles(x)(y+1), "S", "center")
        // N -> S 
        connect(cuBundles(x)(y+1), "S", cuBundles(x)(y), "N", "center")
      }


      /* ----- CU to SB Connection ----- */
      // NW (top left)
      connect(cuBundles(x)(y), "NW", switchBundles(x)(y+1), "SE", "center")
      // NE (top right)
      connect(cuBundles(x)(y), "NE", switchBundles(x+1)(y+1), "SW", "center")
      // SW (bottom left)
      connect(cuBundles(x)(y), "SW", switchBundles(x)(y), "NE", "center")
      // SE (bottom right)
      connect(cuBundles(x)(y), "SE", switchBundles(x+1)(y), "NW", "center")

      // SB to CU
      // NW (top left)
      connect(switchBundles(x)(y+1), "SE", cuBundles(x)(y), "NW", "center")
      // NE (top right)
      connect(switchBundles(x+1)(y+1), "SW", cuBundles(x)(y), "NE", "center")
      // SW (bottom left)
      connect(switchBundles(x)(y), "NE", cuBundles(x)(y), "SW", "center")
      // SE (bottom right)
      connect(switchBundles(x+1)(y), "NW", cuBundles(x)(y), "SE", "center")
    }
  }

  for (y <- 0 until numRows+1) {
    for (x <- 0 until numCols+1) {

      /* ---- SB to SB connections ----*/
      // SB to SB (Horizontal)
      if (x!=numCols) {
        // W -> E 
        connect(switchBundles(x)(y), "E", switchBundles(x+1)(y), "W", "center")
        // E -> W
        connect(switchBundles(x+1)(y), "W", switchBundles(x)(y), "E", "center")
      }
      // SB to SB (Vertical)
      if (y!=numRows) {
        // S -> N
        connect(switchBundles(x)(y), "N", switchBundles(x)(y+1), "S", "center")
        // N -> S 
        connect(switchBundles(x)(y+1), "S", switchBundles(x)(y), "N", "center")
      }

      // Top to SB
      // Top Switches
      if (y==numRows) {
        // S -> N
        connect(switchBundles(x)(y), "N", argBundle, "S", "top") // bottom up 
        // N -> S
        connect(argBundle, "S", switchBundles(x)(y), "N", "top") // top down
      }
      // Bottom Switches
      if (y==0) {
        // N -> S
        connect(switchBundles(x)(y), "S", argBundle, "N", "bottom") // top down 
        // S -> N
        connect(argBundle, "N", switchBundles(x)(y), "S", "bottom") // bottom up
      }


      ///* ---- OCU and SB connection ----*/
      //// OCU to SB 
      //connect(ocuArray(x)(y), "W", switchBundles(x)(y), "E", "center")

      //// SB to OCU
      //connect(switchBundles(x)(y), "E", ocuArray(x)(y), "W", "center")
    }
  }

  /** ----- Fringe Connection ----- **/
  //for (y <- 0 until mcArray.headOption.map(_.size).getOrElse(0)) { //cols
    //for (x <- 0 until mcArray.size) { //rows

      ///* ---- DramAddrGen and SwitchBox connection ---- */
      //if (x==0) {
        //// DAG to SB (W -> E) (left side)
        //connect(dramAGs(x)(y), "E", switchBundles(0)(y), "W", "left")
        //// SB to DAG (E -> W) (left side)
        //connect(switchBundles(0)(y), "W", dramAGs(x)(y), "E", "left")
      //} else {
        //// DAG to SB (E -> W) (right side)
        //connect(dramAGs(x)(y), "W", switchBundles(numCols)(y), "E", "right")
        //// SB to DAG (W -> E) (right side)
        //connect(switchBundles(numCols)(y), "E", dramAGs(x)(y), "W", "right")
      //}

      ///* ---- SramAddrGen and SwitchBox connection ---- */
      //if (x==0) {
        //// SAG to SB (W -> E) (left side)
        //connect(sramAGs(x)(y), "E", switchBundles(0)(y), "W", "left")
        //// SB to SAG (E -> W) (left side)
        //connect(switchBundles(0)(y), "W", sramAGs(x)(y), "E", "left")
      //} else {
        //// SAG to SB (E -> W) (right side)
        //connect(sramAGs(x)(y), "W", switchBundles(numCols)(y), "E", "right")
        //// SB to SAG (W -> E) (right side)
        //connect(switchBundles(numCols)(y), "E", sramAGs(x)(y), "W", "right")
      //}

      ///* ---- MC and SwitchBox connection ---- */
      //if (x==0) {
        //// MC to SB (W -> E) (left side)
        //connect(mcArray(x)(y), "E", switchBundles(0)(y), "W", "left")
        //// SB to MC (E -> W) (left side)
        //connect(switchBundles(0)(y), "W", mcArray(x)(y), "E", "left")
      //} else {
        //// MC to SB (E -> W) (right side)
        //connect(mcArray(x)(y), "W", switchBundles(numCols)(y), "E", "right")
        //// SB to MC (W -> E) (right side)
        //connect(switchBundles(numCols)(y), "E", mcArray(x)(y), "W", "right")
      //}

      ///* ---- MC and DramAddrGen connection ---- */
      //val pos = if (x==0) "left" else "right"
      //// MC to DAG (S -> N)
      //connect(mcArray(x)(y), "N", dramAGs(x)(y), "S", pos)
      //// DAG to MC (N -> S)
      //connect(dramAGs(x)(y), "S", mcArray(x)(y), "N", pos)
    //}
  //}

}
