package spade.codegen

import spade._
import spade.node._

import pirc._
import pirc.codegen._

import scala.collection.mutable.ListBuffer

class SpadeParamCodegen(implicit design: Spade) extends Codegen with ScalaCodegen with MultiFileCodegen {
  def shouldRun = Config.codegen
  import spademeta._
  import design.topParam._
  import design.top._
  import design.top

  val traitName = s"GeneratedParams"
  lazy val dir = sys.env("PLASTICINE_HOME") + "/src/main/scala/spade/gen"
  override lazy val stream = newStream(dir, s"$traitName.scala")

  override def splitPreHeader:Unit = {
    emitHeader
  }

  override def splitPostHeader:Unit = {
    emitln(s"self:TopParams =>")
    emitln(s"import plasticineParams._")
    val fn = if (isSplitting) s"${fileNumber}" else s""
    emitBSln(s"def genParams$fn:Unit = ")
  }

  override def splitPreFooter:Unit = {
    emitBEln
  }

  def emitHeader = {
    emitln(s"package plasticine.spade")
    emitln(s"import chisel3._")
    emitln(s"import chisel3.util._")
    emitln(s"import scala.collection.mutable.ListBuffer")
    emitln(1)
  }

  addPass {
    emitHeader
    emitSplit(emitParams)
    emitTopParams
    emitParamClass
    emitMixed {
      splitPostHeader
      (0 until fileNumber).foreach { i =>
        emitln(s"genParams${i+1}")
      }
      splitPreFooter
    }
  }

  def emitTopParams = {
    emitBlock(s"object GeneratedTopParams extends TopParams with GeneratedParams") {
      emitFringeParam
      emitPlasticineParams
      emitln(s"genParams")
    }
  }

  def emitFringeParam = {
    emitBlock(s"override lazy val fringeParams = new FringeParams") {
      emitln(s"override val numArgIns = ${numArgIns}")
      emitln(s"override val numArgOuts = ${numArgOuts}")
      emitln(s"override val dataWidth = ${wordWidth}")
    }
  }

  def emitPlasticineParams = {
    emitBlock(s"override lazy val plasticineParams = new PlasticineParams") {
      emitln(s"override val w = ${wordWidth}")
      emitln(s"override val numRows = ${numRows}")
      emitln(s"override val numCols = ${numCols}")
      emitln(s"override val cuParams = Array.fill(${cuArray.size})(Array.ofDim[CUParams](${cuArray.head.size}))")
      emitln(s"override val vectorSwitchParams = Array.fill(${sbArray.size})(Array.ofDim[VectorSwitchParams](${sbArray.head.size}))")
      emitln(s"override val scalarSwitchParams = Array.fill(${sbArray.size})(Array.ofDim[ScalarSwitchParams](${sbArray.head.size}))")
      emitln(s"override val controlSwitchParams = Array.fill(${sbArray.size})(Array.ofDim[ControlSwitchParams](${sbArray.head.size}))")
      emitln(s"override val switchCUParams = Array.fill(${sbArray.size})(Array.ofDim[SwitchCUParams](${sbArray.head.size}))")
      emitln(s"override val scalarCUParams = Array.fill(${dramAGs.size})(Array.ofDim[ScalarCUParams](${dramAGs.head.size}))")
      emitln(s"override val memoryChannelParams = Array.fill(${mcArray.size})(Array.ofDim[MemoryChannelParams](${mcArray.head.size}))")
      emitln(s"override val numArgOutSelections = ${quote(top.sins.map(_.fanIns.size))}")
      emitln(s"override val numDoneConnections = ${top.cins.head.fanIns.size}")
    }
  }

  def emitRegs(cu:ComputeUnit) = {
    cu.regs.foreach { reg =>
      emitln(s"regColors += List(${reg.colors.mkString(",")})")
    }
  }

  def emitStages(cu:ComputeUnit) = {
    //emitln(s"val stageTypes = ListBuffer[StageType]()")
    //cu.stages.foreach { stage =>
      //emitln(s"stageTypes += ${quote(stage)}")
    //}
    cu match {
      case cu:MemoryComputeUnit =>
        emitln(s"override val d = ${cu.stages.size}")
        //emitln(s"override val wd = ${cu.wastages.size}")
      case cu =>
        emitln(s"override val d = ${cu.stages.size}")
    }
  }

  def emitParamClass = {
    pcus.headOption.foreach { pcu =>
      emitBlock(s"case class GeneratedPCUParams(override val numScalarIn:Int, override val numScalarOut:Int, override val numVectorIn:Int, override val numVectorOut:Int, override val numControlIn:Int, override val numControlOut:Int) extends PCUParams") {
        emitln(s"override val w = ${wordWidth}")
        emitln(s"override val v = ${numLanes}")
        emitln(s"override val numCounters = ${pcu.param.numCtrs}")
        emitln(s"override val numUDCs = ${pcu.ctrlBox.param.numUDCs}")
        emitRegs(pcu)
        emitStages(pcu)
        emitln(s"override val r = regColors.size")
      }
    }
    emitln(1)

    mcus.headOption.foreach { mcu =>
      emitBlock(s"case class GeneratedPMUParams(override val numScalarIn:Int, override val numScalarOut:Int, override val numVectorIn:Int, override val numVectorOut:Int, override val numControlIn:Int, override val numControlOut:Int) extends PMUParams") {
        emitln(s"override val w = ${wordWidth}")
        emitln(s"override val v = ${numLanes}")
        emitln(s"override val numCounters = ${mcu.param.numCtrs}")
        emitln(s"override val numUDCs = ${mcu.ctrlBox.param.numUDCs}")
        emitRegs(mcu)
        emitStages(mcu)
        emitln(s"override val r = regColors.size")
      }
    }

    ocus.foreach { ocu =>
      emitBlock(s"case class GeneratedSwitchCUParams(override val numScalarIn:Int, override val numControlIn:Int, override val numControlOut:Int) extends SwitchCUParams") {
        emitln(s"override val w = ${wordWidth}")
        emitln(s"override val numCounters = ${ocu.param.numCtrs}")
        emitln(s"override val numUDCs = ${ocu.ctrlBox.param.numUDCs}")
        emitln(s"override val numScalarOut = 0")
      }
    }

    dramAGs.headOption.foreach { _.headOption.foreach { scu =>
      emitBlock(s"case class GeneratedScalarCUParams(override val numScalarIn:Int, override val numScalarOut:Int, override val numControlIn:Int, override val numControlOut:Int) extends ScalarCUParams") {
        emitln(s"override val w = ${wordWidth}")
        emitln(s"override val numCounters = ${scu.param.numCtrs}")
        emitln(s"override val numUDCs = ${scu.ctrlBox.param.numUDCs}")
        emitRegs(scu)
        emitStages(scu)
        emitln(s"override val r = regColors.size")
      }
    }}

    mcs.headOption.foreach { mc =>
      emitBlock(s"case class GeneratedMemoryChannelParams(override val numScalarIn:Int, override val numControlIn:Int, override val numControlOut:Int) extends MemoryChannelParams") {
        emitln(s"override val w = ${wordWidth}")
        emitln(s"override val v = ${numLanes}")
      }
    }

  }

  def emitParams = {
    cuArray.foreach { case row =>
      row.foreach { case cu =>
        val param = cu match {
          case mcu:MemoryComputeUnit => 
            s"GeneratedPMUParams(numScalarIn=${cu.sins.size}, numScalarOut=${cu.souts.size}, numVectorIn=${cu.vins.size}, numVectorOut=${cu.vouts.size}, numControlIn=${cu.cins.size}, numControlOut=${cu.couts.size})"
          case cu:PatternComputeUnit => 
            s"GeneratedPCUParams(numScalarIn=${cu.sins.size}, numScalarOut=${cu.souts.size}, numVectorIn=${cu.vins.size}, numVectorOut=${cu.vouts.size}, numControlIn=${cu.cins.size}, numControlOut=${cu.couts.size})"
          case scu:ScalarComputeUnit =>
            s"GeneratedScalarCUParams(numScalarIn=${cu.sins.size}, numScalarOut=${cu.souts.size}, numControlIn=${cu.cins.size}, numControlOut=${cu.couts.size})"
        }
        emitln(s"${quote(cu)} = $param")
      }
    }

    ocuArray.foreach { row =>
      row.foreach { cu =>
        val param = s"GeneratedSwitchCUParams(numScalarIn=${cu.sins.size}, numControlIn=${cu.cins.size}, numControlOut=${cu.couts.size})"
        emitln(s"${quote(cu)} = $param")
      }
    }

    dramAGs.foreach { row =>
      row.foreach { cu =>
        val param = s"GeneratedScalarCUParams(numScalarIn=${cu.sins.size}, numScalarOut=${cu.souts.size}, numControlIn=${cu.cins.size}, numControlOut=${cu.couts.size})"
        emitln(s"${quote(cu)} = $param")
      }
    }

    mcArray.foreach { row =>
      row.foreach { cu =>
        val param = s"GeneratedMemoryChannelParams(numScalarIn=${cu.sins.size}, numControlIn=${cu.cins.size}, numControlOut=${cu.couts.size})"
        emitln(s"${quote(cu)} = $param")
      }
    }

    sbArray.foreach { row =>
      row.foreach { sb =>
        emitln(s"${qv(sb)} = VectorSwitchParams(numIns=${sb.vins.size}, numOuts=${sb.vouts.size}, v=${numLanes}, w=${wordWidth})")
        emitln(s"${qs(sb)} = ScalarSwitchParams(numIns=${sb.sins.size}, numOuts=${sb.souts.size}, w=${wordWidth})")
        emitln(s"${qc(sb)} = ControlSwitchParams(numIns=${sb.cins.size}, numOuts=${sb.couts.size})")
      }
    }

  }

  def quote(n:Node):String = n match {
    case n:Stage => s"FUStage(numOprds=${n.param.numOprds}, ops=${quote(n.param.ops)})"
    case n:ScalarComputeUnit =>
      val (x, y) = coordOf(n)
      x match {
        case -1 => s"scalarCUParams(0)($y)"
        case `numCols` => s"scalarCUParams(1)($y)"
        case _ => s"cuParams($x)($y)"
      }
    case n:MemoryController =>
      val (x, y) = coordOf(n)
      x match {
        case -1 => s"memoryChannelParams(0)($y)"
        case `numCols` => s"memoryChannelParams(1)($y)"
      }
    case n:MemoryComputeUnit =>
      val (x, y) = coordOf(n)
      s"cuParams($x)($y)"
    case n:OuterComputeUnit =>
      val (x, y) = coordOf(n)
      s"switchCUParams($x)($y)"
    case n:ComputeUnit =>
      val (x, y) = coordOf(n)
      s"cuParams($x)($y)"
  }

  def qv(n:Any):String = n match {
    case n:SwitchBox =>
      val (x, y) = coordOf(n)
      s"vectorSwitchParams($x)($y)"
    case n => quote(n)
  }

  def qs(n:Any):String = n match {
    case n:SwitchBox =>
      val (x, y) = coordOf(n)
      s"scalarSwitchParams($x)($y)"
    case n => quote(n)
  }

  def qc(n:Any):String = n match {
    case n:SwitchBox =>
      val (x, y) = coordOf(n)
      s"controlSwitchParams($x)($y)"
    case n => quote(n)
  }

}
