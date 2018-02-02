package spade.codegen 

import spade._
import spade.node._
import spade.traversal._

import pirc._
import pirc.util._
import pirc.enums._
import pirc.codegen._

import scala.collection.mutable.ListBuffer

// Compiler Passes defined in spade/src/core/Spade.scala
// Control nodes defined in spade/src/node/Control.scala 
// To run spade compiler. Do bin/spade <Arch> in pir-compiler folder. e.g. 
// "bin/spade SN2x2 --codegen" for 2x2 Plasticine. 
// you can also run "spade arch.SN2x2 --codegen" in sbt console for faster incremental compilation
// All possible plasticine configurations are defined in spade/arch/src. 
// SNs.scala defines default networks. SN_Preloads.scala define config which loads some of the configuration from a config
// file.
// Codegen is turned off by default unless use "--codegen". To change default change in pirc/src/core/Config.scala

class SpadeControlCodegen(implicit design: Spade) extends Codegen with ScalaCodegen with HiearchicalTraversal {
  def shouldRun = Config.codegen
  import design.spademeta._ // metadata information about the IR. defined spade/src/util/SpadeMetadata.scala
  import design.topParam._ // defined in spade/src/node/Top.scala

  val traitName = s"GeneratedControlBox"
  lazy val dir = sys.env("PLASTICINE_HOME") + "/src/main/scala/arch/gen"
  override lazy val stream = newStream(dir, s"$traitName.scala") 

  def emitHeader = {
    emitln(s"package plasticine.arch")
    emitln(s"import chisel3._")
    emitln(s"import chisel3.util._")
    emitln(s"import plasticine.templates.MuxN")
    emitln(s"import scala.language.reflectiveCalls")
    emitln(s"import scala.collection.mutable.ListBuffer")
    emitln(1)
  }

  addPass(canRun=true, runCount=1) {
    emitHeader
    val prts = design.top.prts // all controllers

    val top = design.top // top level modules
    val pcu = design.top.pcus.head
    val pmu = design.top.pmus.head
    val scu = design.top.scus.head // scalar compute unit. Only have a single lane in pipeline stages
    val ocu = design.top.ocus.head // switch cus. contains a ControlBox, some registers and counters. 
    val mc = design.top.mcs.head // Memory controller interface

    traverseDown(top.ctrlBox)
    traverseDown(pcu.ctrlBox)
    traverseDown(pmu.ctrlBox)
    traverseDown(scu.ctrlBox)
    traverseDown(ocu.ctrlBox)
    traverseDown(mc.ctrlBox)

    emitTitleComment("Entire Plasticine IR")
    traverseDown(design.top)
  }

  // emitln(string) prints line in file with intentation. emit(text) print without return line.
  // emitBlock(text1) {
  //    emitln(text2)
  //    emitln(text3)
  // }
  // prints in file
  // text1 {
  //    text2
  //    text3
  // }
  // Some other useful scala syntex codegen are defined in pirc/src/codegen/ScalaCodegen.scala.
  // Welcome to add more common scala codegen utility function in there.
  //
  // traverse from top level modules to submodules.
  override def traverseDown(node:Any):Unit = {
    // Each module/input/output have a unique integer ID if you need to name unique wire. 
    node match {
      case n:Input[_,_] =>
        emitln(s"$n fanIn=${n.fanIns}") // connected outputs
      case n:Output[_,_] =>
        emitln(s"$n fanOut=${n.fanOuts}") // connected inputs. When a input is connected to multiple outputs, there is a configuration mux for that input 
      case n:Module => emitBlock(s"$n") { 
        super.traverseDown(n)
      }
    }
  }


}
