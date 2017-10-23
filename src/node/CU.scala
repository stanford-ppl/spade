package spade.node

import spade._
import spade.util._

import pirc.util._
import pirc.exceptions._
import pirc.enums._

import scala.collection.mutable.ListBuffer

trait ComputeUnitParam extends ControllerParam {
  val numRegs:Int
  val numCtrs:Int
  val numLanes:Int
  val numStages:Int
  val ops:List[Op]
  val numVouts:Int
  val numSouts:Int
  val numCouts:Int
  val reduction:Boolean = false
}

/*
 * ComputeUnit
 * */
abstract class ComputeUnit(override val param:ComputeUnitParam)(implicit spade:Spade) extends Controller(param) {

  import spademeta._
  import param._

  /* ---------- SUBMODULES -----------*/
  //override implicit val ctrler:ComputeUnit = this 
  val regs:List[ArchReg] = List.tabulate(numRegs) { ir => ArchReg().index(ir) }
  val ctrs:List[Counter] = List.tabulate(numCtrs) { i => Module(Counter()).index(i) }
  def vout = vouts.head

  protected val _regstages:ListBuffer[Stage] = ListBuffer.empty  // Regular Stages
  protected val _rdstages:ListBuffer[ReduceStage] = ListBuffer.empty // Reduction Stages
  protected val _stages:ListBuffer[Stage] = ListBuffer.empty // All stages

  def regstages:List[Stage] = _regstages.toList // Regular Stages
  def rdstages:List[ReduceStage] = _rdstages.toList // Reduction Stages
  def stages:List[Stage] = _stages.toList // All stages

  def addRegstages(stages:List[Stage]) = { _regstages ++= stages; addStages(stages) }
  def addRdstages(stages:List[ReduceStage]) = { _rdstages ++= stages; addStages(stages) }

  protected def addStages(sts:List[Stage]) = {
    sts.zipWithIndex.foreach { case (stage, i) =>
      stage.index(stages.size)
      if (stages.nonEmpty) {
        stage.prev = Some(stages.last)
        stage.prev.get.next = Some(stage)
      }
      _stages += stage
    }
  }

  def addRegstages(numStage:Int, numOprds:Int, ops:List[Op]):this.type = { 
    addRegstages(List.fill(numStage) { Module(new Stage(StageParam(numOprds, regs, ops))) }); this // Regular stages
  }
  def addRdstages(numStage:Int, numOprds:Int, ops:List[Op]):this.type = {
    addRdstages(List.fill(numStage) { Module(ReduceStage(StageParam(numOprds, regs, ops))) }); this // Reduction stage 
  } 

  def color(range:Range, color:RegColor):this.type = { range.foreach { i => regs(i).color(color) }; this }
  def color(i:Int, color:RegColor):this.type = { regs(i).color(color); this }

  /* ---------- SIMULATION -----------*/
  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    super.register
  }

  override def connect:Unit = {
    super.connect
    warn(souts.size < numSouts, s"${quote(this)} souts=${souts.size} numSouts=${numSouts}")
    warn(vouts.size < numVouts, s"${quote(this)} vouts=${vouts.size} numVouts=${numVouts}")
    val numReduceStages = if (reduction) Math.ceil(Math.log(numLanes) / Math.log(2)).toInt else 0
    val numFrontStages = if (reduction) numStages - (numReduceStages + 2) else numStages
    assert(numFrontStages >= 0, s"numFrontStages=$numFrontStages numStage=$numStages")
    addRegstages(numStage=numFrontStages, numOprds=3, ops)
    addRdstages(numStage=numReduceStages, numOprds=3, ops)
    if (reduction) addRegstages(numStage=2, numOprds=3, ops)

    if (stages.nonEmpty) {
      setColors
      connectPipeline
    }
    connectSRAM
    connectCounters
  }

  def setColors = {
    color(0 until numCtrs, CounterReg)
    if (reduction) color(0, ReduceReg)
    color(1, AccumReg)
    color(numRegs-numScalarFifos until numRegs, ScalarInReg)
    color(numRegs-souts.size until numRegs, ScalarOutReg)
    color(numRegs-numVectorFifos until numRegs, VecInReg)
    color(numRegs-vouts.size until numRegs, VecOutReg)
  }

  def forwardStages = this match {
    //case cu:PatternMemoryUnit => cu.wastages ++ cu.rastages.headOption.map{ h => List(h) }.getOrElse(Nil)
    case cu:PatternMemoryUnit => cu.stages
    case cu:OuterComputeUnit => Nil
    case cu:ComputeUnit => List(cu.stages.head)
  }

  def connectPipeline = {
    for (i <- 1 until stages.size) {
      regs.foreach { reg => stages(i).get(reg).in <== stages(i-1).get(reg).out }
    }

    // Bus input is forwarded to 1 register in empty stage
    val viRegs = regs.filter(_.is(VecInReg))
    assert(vins.size == vfifos.size, s"cu:${this} vins:${vins.size} vfifos:${vfifos.size}")
    if (stages.nonEmpty) assert(vfifos.size == viRegs.size)
    (vfifos, viRegs).zipped.foreach { case (fifo, reg) =>
      forwardStages.foreach { s => s.get(reg).in <== fifo.readPort }
    }

    // One to one
    val siRegs = regs.filter(_.is(ScalarInReg))
    (sfifos, siRegs).zipped.foreach { case (sbuf, reg) =>
      forwardStages.foreach { s => s.get(reg).in <-- sbuf.readPort } // broadcast
    }

    // Counters can be forwarde to empty stage, writeAddr and readAddr stages 
    (ctrs, regs.filter(_.is(CounterReg))).zipped.foreach { case (c, reg) => 
      forwardStages.foreach { s => s.get(reg).in <== c.out }
    }

    val voRegs = regs.filter(_.is(VecOutReg))
    //assert(vouts.size == voRegs.size, s"cu:${cu} vouts:${vouts.size} voRegs:${voRegs.size}")
    //(vouts, voRegs).zipped.foreach { case (vo, reg) => vo.ic <== stages.last.get(reg).out }
    // Xbar
    vouts.foreach { vout => voRegs.foreach { reg => vout.ic <== stages.last.get(reg).out } }
    // Xbar
    val soRegs = regs.filter(_.is(ScalarOutReg))
    souts.foreach { sout => soRegs.foreach { reg => sout.ic <== (stages.last.get(reg).out, 0) } }

    /* FU Constrain  */
    stages.foreach { stage =>
      // All stage can read from any regs of its own stage, previous stage, and Const
      stage.funcUnit.operands.foreach{ oprd =>
        oprd <-- Const().out // operand is constant
        regs.foreach{ reg =>
          oprd <== stage.get(reg) // operand is from current register block
          stage.prev.foreach { oprd <== _.get(reg) }// operand is forwarded from previous register block
        }
      }
      // All stage can write to all regs of its stage
      regs.foreach{ reg => stage.get(reg) <== stage.funcUnit.out }
    }

    forwardStages.foreach { stage =>
      stage.funcUnit.operands.foreach { oprd => 
        ctrs.foreach{ oprd <== _.out }
        srams.foreach { oprd <== _.readPort }
        vfifos.foreach { oprd <== _.readPort }
        sfifos.foreach { oprd <-- _.readPort }
      }
    }
  }

  def connectSRAM:Unit = {
    srams.foreach { sram =>
      sram.readAddrMux.inputs.foreach { _ <== (ctrs.map(_.out), 0) }// sram read/write addr can be from all counters
      sram.readAddrMux.inputs.foreach { _ <== sfifos.map(_.readPort) }
      sram.writeAddrMux.inputs.foreach { _ <== (ctrs.map(_.out), 0) }
      sram.writeAddrMux.inputs.foreach { _ <== sfifos.map(_.readPort) }
      stages.foreach { stage => 
        sram.readAddrMux.inputs.foreach { _ <== (stage.funcUnit.out, 0) }
        sram.writeAddrMux.inputs.foreach { _ <== (stage.funcUnit.out, 0) }
      }
      sram.writePortMux.inputs.foreach { _ <== vfifos.map(_.readPort) }
      sfifos.foreach { sbuf => sram.writePortMux.inputs.foreach { _.sliceHead(sbuf.readPort) } }
    } 
  }

  def connectCounters:Unit = {
    ctrs.foreach { c => 
      c.min <== Const().out // Counter max, min, step can be constant or scalarIn(specified later)
      c.min <== sfifos.map(_.readPort)
      c.max <== Const().out
      c.max <== sfifos.map(_.readPort)
      c.step <== Const().out
      c.step <== sfifos.map(_.readPort)
    }
    /* Chain counters together */
    for (i <- 1 until ctrs.size) { ctrs(i).en <== ctrs(i-1).done } 
    for (i <- 0 until ctrs.size by 1) { isInnerCounter(ctrs(i)) = true  } // Allow group counter in chain in multiple of 2

    (this, ctrlBox) match {
      case (cu:ComputeUnit, cb:InnerCtrlBox) => 
        ctrs.foreach { cb.done.in <== _.done }
        ctrs.filter { ctr => isInnerCounter(ctr) }.map(_.en <== cb.en.out)
      case (cu:OuterComputeUnit, cb:OuterCtrlBox) => 
        ctrs.foreach { cb.done.in <== _.done }
        ctrs.filter { ctr => isInnerCounter(ctr) }.map(_.en <== cb.en.out)
      case (cu:PatternMemoryUnit, cb:MemoryCtrlBox) => 
        ctrs.foreach { cb.readDone.in <== _.done }
        //cins.foreach { cb.readDone.in <== _.ic }
        ctrs.foreach { cb.writeDone.in <== _.done }
        //cins.foreach { cb.writeDone.in <== _.ic }
        ctrs.filter { ctr => isInnerCounter(ctr) }.map(_.en <== cb.readEn.out)
        ctrs.filter { ctr => isInnerCounter(ctr) }.map(_.en <== cb.writeEn.out)
    }
  }


}

