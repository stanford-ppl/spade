package spade.node

import spade._
import spade.util._

import pirc.enums._
import pirc.util._

import scala.collection.mutable.ListBuffer

abstract class LUT(implicit spade:Spade) extends Primitive {
  val numIns:Int
}
case class EnLUT(numIns:Int)(implicit spade:Spade) extends LUT {
  import spademeta._
  override val typeStr = "enlut"
  override def toString =s"${super.toString}${indexOf.get(this).fold(""){idx=>s"[$idx]"}}"
}
case class TokenOutLUT()(implicit spade:Spade) extends LUT{
  import spademeta._
  override val typeStr = "tolut"
  override def toString =s"${super.toString}${indexOf.get(this).fold(""){idx=>s"[$idx]"}}"
  override val numIns = 2 // Token out is a combination of two output
}
case class TokenDownLUT(numIns:Int)(implicit spade:Spade) extends LUT {
  override val typeStr = "tdlut"
}
object TokenDownLUT {
  def apply(idx:Int, numIns:Int)(implicit spade:Spade):TokenDownLUT = 
    TokenDownLUT(numIns).index(idx)
}

case class UpDownCounterConfig (
  initVal:Int,
  name:String
) extends Configuration

case class UpDownCounter()(implicit spade:Spade) extends Primitive with Simulatable 
with Configurable {
  import spademeta._
  type CT = UpDownCounterConfig
  override val typeStr = "udc"

  /* ---------- IOs -----------*/
  val inc = Input(Bit(), this, s"${this}.inc")
  val dec = Input(Bit(), this, s"${this}.dec")
  val count = Output(Word(), this, s"${this}.count")
  val out = Output(Bit(), this, s"${this}.out")

  lazy val ctrlBox = collectUp[CtrlBox](this).headOption

  /* ---------- SIMULATION -----------*/
  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    cfmap.get(this).foreach { config =>
      dprintln(s"${quote(this)} -> ${config.name} initVal=${config.initVal}")
      count.v.default = config.initVal
      count.v.set { countv =>
        if (rst) {
          countv <<= config.initVal
        } else {
          If (inc.pv) { countv <<= countv + 1 }
          If (dec.pv) {
            if (sim.inSimulation && countv.toInt==0) {
              warn(s"${quote(this)}(${config.name}) of ${quote(prt)} underflow at cycle #$cycle")
            }
            countv <<= countv - 1
          }
        }
      }
      out.v := (count.v > 0)
    }
  }
}

case class AndGate()(implicit spade:Spade) extends Primitive with Simulatable {

  /* ---------- IOs -----------*/
  val out = Output(Bit(), this, s"${this}.out")
  private[spade] def <== (outs:List[Output[Bit, Module]]):Unit = outs.foreach { out => <==(out) }
  private[spade] def <== (out:Output[Bit, Module]):Unit = {
    val i = ins.size
    val in = Input(Bit(), this, s"${this}.in$i").index(i)
    in <== out
  }

  /* ---------- SIMULATION -----------*/
  override def register(implicit sim:Simulator):Unit = {
    val invs = ins.map(_.v).collect{ case v:SingleValue => v }
    out.v := {
      val res = invs.map{ _.update.value }.reduceOption[Option[AnyVal]]{ case (in1, in2) => 
        eval(BitAnd, in1, in2)
      }
      res.getOrElse(None)
    }
  }
}
object AndGate {
  def apply(name: => String = "ag")(implicit spade:Spade):AndGate = {
    new AndGate() { override def typeStr = name }
  }
}
case class AndTree(name:Option[String])(implicit spade:Spade) extends Primitive with Simulatable {
  import spademeta._
  override val typeStr = name.getOrElse("at")
  /* ---------- IOs -----------*/
  val out = Output(Bit(), this, s"${this}.out")
  private[spade] def <== (outs:List[Output[Bit, Module]]):Unit = outs.foreach { out => <==(out) }
  private[spade] def <== (out:Output[Bit, Module]):Unit = {
    val i = ins.size
    val in = Input(Bit(), this, s"${this}.in$i").index(i)
    in <== Const(true).out
    in <== out
  }

  /* ---------- SIMULATION -----------*/
  override def register(implicit sim:Simulator):Unit = {
    val invs = ins.map(_.v).collect{ case v:SingleValue => v }
    out.v := {
      val res = invs.map{_.update.value }.reduceOption[Option[AnyVal]]{ case (in1, in2) => 
        eval(BitAnd, in1, in2)
      }
      res.getOrElse(None)
    }
  }
}
object AndTree {
  def apply(name:String)(implicit spade:Spade):AndTree = AndTree(Some(name))
  def apply()(implicit spade:Spade):AndTree = AndTree(None)
}

case class PulserSMConfig (
  pulserLength:Int
) extends Configuration

case class PulserSM()(implicit spade:Spade) extends Primitive with Simulatable with Configurable {
  type CT = PulserSMConfig

  override lazy val prt:OuterComputeUnit = collectUp[OuterComputeUnit](this).head

  /* ---------- IOs -----------*/
  val done = Input(Bit(), this, s"${this}.done")
  val en = Input(Bit(), this, s"${this}.en")
  val init = Input(Bit(), this, s"${this}.init")
  val out = Output(Bit(), this, s"${this}.out")
  val INIT = false
  val RUNNING = true
  val state = Output(Bit(), this, s"${this}.state")
  var pulseLength = 1

  /* ---------- SIMULATION -----------*/
  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    cfmap.get(prt).foreach { cuconfig:OuterComputeUnitConfig =>
      val config = cfmap(this)
      if (cuconfig.isSeq || cuconfig.isMeta) {
        state.v.default = INIT 
        out.v.set { outv =>
          If (state.v =:= INIT) {
            If(init.v) {
              outv.setHigh
              pulseLength = config.pulserLength// lengthOf(cu) / pipelinedBy(cu)(sim.design)
              state.v <<= RUNNING
            }
          } 
          If(state.v =:= RUNNING) {
            IfElse (done.v) {
              state.v <<= INIT
            } {
              If (en.pv) {
                outv.setHigh
                pulseLength = 1 
              }
            }
          } 
          If(out.vAt(pulseLength)) { outv.setLow }
        }
      }
    }
  }
}

case class UpDownSM()(implicit spade:Spade) extends Primitive with Simulatable {

  /* ---------- IOs -----------*/
  val doneIn = Input(Bit(), this, s"${this}.doneIn")
  val inc = Input(Bit(), this, s"${this}.inc")
  val dec = Input(Bit(), this, s"${this}.dec")
  val doneOut = Output(Bit(), this, s"${this}.doneOut")
  val notDone = Output(Bit(), this, s"${this}.notDone")
  val notRun = Output(Bit(), this, s"${this}.notRun")
  val finished = Output(Bit(), this, s"${this}.finished")
  // Internal signals 
  val out = Output(Bit(), this, s"${this}.out")
  val count = Output(Word(), this, s"${this}.count")
  val done = Output(Bit(), this, s"${this}.done") // Initially low
  val udc = Module(UpDownCounter()).index(-1)

  /* ---------- SIMULATION -----------*/
  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    if (isMapped(this)) {
      done.v.default = false 
      done.v.set { donev =>
        If (doneIn.v) { donev.setHigh }
        If (doneOut.v) { donev.setLow }
      }
      notDone.v := done.v.not
      udc.inc.v := inc.v
      udc.dec.v := dec.v
      count.v := udc.count.v
      out.v := udc.out.v
      notRun.v := out.v.not 
      finished.v := (done.pv & notRun.pv)
      doneOut.v.set { doneOutv =>
        Match(
          (finished.v & finished.pv.not) -> { () => doneOutv.setHigh },
          doneOut.pv -> { () => doneOutv.setLow }
        ) { doneOutv.setLow }
      } 
    }
  }
}

case class PredicateUnitConfig  (
  op:Op,
  const:Int
) extends Configuration

case class PredicateUnit(name:String)(implicit spade:Spade) extends Primitive 
  with Simulatable with Configurable {

  type CT = PredicateUnitConfig

  override val typeStr = s"pu_$name"

  /* ---------- IOs -----------*/
  val in = Input(Word(), this, s"${quote(this)}.in")
  val out = Output(Bit(), this, s"${quote(this)}.out")

  /* ---------- SIMULATION -----------*/
  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    cfmap.get(this).fold {
      out.v := false
    } { config => 
      out.v := eval(config.op, in.v, config.const)
    }
  }
}


case class CtrlBoxParam (
  numUDCs:Int = 5
) extends SpadeParam
abstract class CtrlBox(val param:CtrlBoxParam)(implicit spade:Spade) extends Primitive with Simulatable {
  import spademeta._

  override lazy val prt:Controller = collectUp[Controller](this).head

  /* ---------- SUBMODULES -----------*/
  val fifoAndTree = Module(AndTree("fifoAndTree"))
  //val predicateUnits = ListBuffer[PredicateUnit]()
  lazy val delays = collectDown[Delay[Bit]](this)
  lazy val udcs = collectDown[UpDownCounter](this).filter{ n => indexOf.get(n).nonEmpty }.toList.sortBy { _.index }

  /* ----- CONNECTION ----- */
  override def connect = {
    fifoAndTree <== prt.fifos.map(_.notEmpty) 
  }

  /* ---------- SIMULATION -----------*/
  override def register(implicit sim:Simulator):Unit = {
    delays.foreach { delay =>
      delay.in.v.default = false
      delay.out.v.default = false
    }
  }
}

abstract class StageCtrlBox(param:CtrlBoxParam)(implicit spade:Spade) extends CtrlBox(param) {
  import param._
  import spademeta._
  override lazy val prt:ComputeUnit = collectUp[ComputeUnit](this).head

  /* ---------- SUBMODULES -----------*/
  val en = Module(Delay(Bit(), 0, s"${quote(prt)}.en"))
  val done = Module(Delay(Bit(), 0, s"${quote(prt)}.done"))

  for (i <- 0 until numUDCs) { 
    Module(UpDownCounter().index(i))
  }
  val siblingAndTree = AndTree("siblingAndTree") 

  /* ----- CONNECTION ----- */
  override def connect = {
    super.connect
    siblingAndTree <== udcs.map(_.out)

    /* Memory Control Connections */
    prt.fifos.foreach { fifo =>
      fifo.dequeueEnable <== prt.ctrs.map(_.done)
      fifo.dequeueEnable <== en.out; 
      fifo.enqueueEnable <== fifo.writePortMux.valid 
      fifo.predicate <== spade.top.low.out 
      this match {
        case cb:InnerCtrlBox =>
          fifo.predicate <== cb.fifoPredUnit.out
        case _ =>
          fifo.predicate <== spade.top.low.out
      }
    }
    // Scalar FIFO can map registers
    prt.sfifos.foreach { buf =>
      buf.enqueueEnable <== prt.cfifos.map(_.readPort)
    }
  }
}

class InnerCtrlBox(param:CtrlBoxParam)(implicit spade:Spade) extends StageCtrlBox(param) {

  /* ---------- SUBMODULES -----------*/
  val enAnd = Module(AndGate("enAnd"))
  lazy val enDelay = Module(Delay(Bit(), prt.stages.size, s"${quote(prt)}.enDelay"))
  lazy val doneDelay = Module(Delay(Bit(), prt.stages.size, s"${quote(prt)}.doneDelay"))

  val tokenInXbar = Module(Delay(Bit(), 0))
  val tokenInAndTree = Module(AndTree("tokenInAndTree"))

  val accumPredUnit = Module(PredicateUnit("accum"))
  val fifoPredUnit = Module(PredicateUnit("fifo"))

  /* ----- CONNECTION ----- */
  override def connect = {
    doneDelay.in <== done.out
    enDelay.in <== en.out
    tokenInXbar.in <== prt.cins.map(_.ic)
    tokenInAndTree <== prt.cins.map(_.ic)
    enAnd <== siblingAndTree.out
    enAnd <== tokenInAndTree.out
    enAnd <== fifoAndTree.out

    en.in <== enAnd.out
    prt.ctrs.foreach { ctr => 
      accumPredUnit.in <== (ctr.out, 0)
      fifoPredUnit.in <== (ctr.out, 0) 
    }

    udcs.foreach { udc =>
      udc.inc <== prt.cins.map{_.ic}
      udc.dec <== done.out
      udc.dec <== en.out
    }
    /* Memory Control Connections */

    /* Control IO */
    prt.couts.foreach { cout => 
      cout.ic <== prt.fifos.map(_.notFull)
      cout.ic <== doneDelay.out
      cout.ic <== enDelay.out
    }
    prt.gouts.foreach { out =>
      out.valid <== en.out
      out.valid <== prt.ctrs.map(_.done)
    }
    /* Stage Control */
    prt.stages.foreach { _.prs.foreach { _.en <== en.out } }
    super.connect
  }
}

class OuterCtrlBox(param:CtrlBoxParam)(implicit spade:Spade) extends StageCtrlBox(param) {
  import spademeta._
  override lazy val prt:OuterComputeUnit = collectUp[OuterComputeUnit](this).head

  val childrenAndTree = Module(AndTree("childrenAndTree"))
  val udsm = Module(UpDownSM())
  val enAnd = Module(AndGate("enAnd"))

  /* ----- CONNECTION ----- */
  override def connect = {
    childrenAndTree <== udcs.map(_.out)
    udsm.doneIn <== done.out
    udsm.dec <== childrenAndTree.out
    udsm.inc <== en.out
    enAnd <== udsm.notDone
    enAnd <== udsm.notRun
    enAnd.ins(1).asBit <== Const(true).out
    enAnd <== siblingAndTree.out

    en.in <== enAnd.out 

    udcs.foreach { udc =>
      udc.inc <== prt.cins.map{_.ic}
      udc.dec <== childrenAndTree.out
      udc.dec <== done.out
    }
    /* Control IO */
    prt.couts.foreach { cout => 
      cout.ic <== udsm.doneOut 
      cout.ic <== en.out
    }
    prt.gouts.foreach { _.valid <== en.out }
    super.connect
  }
}

class MemoryCtrlBox(param:CtrlBoxParam)(implicit spade:Spade) extends CtrlBox(param) {
  override lazy val prt:PatternMemoryUnit = collectUp[PatternMemoryUnit](this).head

  val tokenInXbar = Module(Delay(Bit(), 0, s"$prt.tokenInXbar"))
  val writeFifoAndTree = Module(AndTree("writeFifoAndTree"))
  val readFifoAndTree = Module(AndTree("readFifoAndTree"))
  val tokenInAndTree = Module(AndTree("tokenInAndTree"))

  //val readUDC = UpDownCounter()

  val readAndGate = Module(AndGate(s"$prt.readAndGate"))

  val readEn = Module(Delay(Bit(),0, s"$prt.readEn"))
  val writeEn = Module(Delay(Bit(), 0, s"$prt.writeEn"))

  val readEnDelay = Module(Delay(Bit(),0, s"$prt.readEnDelay"))
  val writeEnDelay = Module(Delay(Bit(),0, s"$prt.writeEnDelay"))

  val readDone = Module(Delay(Bit(), 0, s"$prt.readDone"))
  val writeDone = Module(Delay(Bit(), 0, s"$prt.writeDone"))

  val readDoneDelay = Module(Delay(Bit(), 0, s"$prt.readDoneDelay"))
  val writeDoneDelay = Module(Delay(Bit(), 0, s"$prt.writeDoneDelay"))

  /* ----- CONNECTION ----- */
  override def connect = {
    super.connect
    tokenInAndTree <== prt.cins.map(_.ic)
    //readAndGate <== readUDC.out
    readAndGate <== tokenInAndTree.out
    readAndGate <== readFifoAndTree.out 
    readEn.in <== readAndGate.out
    writeEn.in <== writeFifoAndTree.out
    readEnDelay.in <== readEn.out
    writeEnDelay.in <== writeEn.out
    readDoneDelay.in <== readDone.out
    writeDoneDelay.in <== writeDone.out

    /* Memory Control Connections */
    (prt.fifos).foreach { fifo => 
      fifo.dequeueEnable <== readEn.out
      fifo.dequeueEnable <== writeEn.out
      fifo.enqueueEnable <== fifo.writePortMux.valid 
      fifo.predicate <== spade.top.low.out
    }
    prt.srams.foreach { sram => 
      sram.enqueueEnable <== prt.cfifos.map(_.readPort) 
      sram.dequeueEnable <== prt.cfifos.map(_.readPort) 
      sram.enqueueEnable <== prt.ctrs.map(_.done) //TODO
      sram.dequeueEnable <== prt.ctrs.map(_.done)
      //sram.dequeueEnable <== readDoneDelay.out 
      //sram.enqueueEnable <== writeDoneDelay.out
      //sram.writeEn <== writeEnDelay.out
      //sram.readEn <== readEnDelay.out
      sram.writeEn <== writeEn.out
      sram.readEn <== readEn.out
    }
    writeFifoAndTree <== prt.fifos.map(_.notEmpty) :+ prt.sram.notFull
    readFifoAndTree <== (prt.fifos.map(_.notEmpty) :+ prt.sram.notEmpty)

    /* Control IO */
    tokenInXbar.in <== prt.cins.map(_.ic)
    prt.couts.foreach { cout => 
      cout.ic <== prt.fifos.map(_.notFull)
      cout.ic <== writeDone.out
      cout.ic <== readDone.out
    }
    prt.gouts.foreach { _.valid <== readEnDelay.out }

    /* Stage Control */
    prt.stages.foreach { _.prs.foreach { pr =>
      pr.en <== writeEn.out
      pr.en <== readEn.out
    } }

  }
}

case class TopCtrlBox(override val param:CtrlBoxParam)(implicit spade:Spade) extends CtrlBox(param) {

  override lazy val prt:Top = collectUp[Top](this).head

  /* ---------- IOs -----------*/
  val command = Output(Bit(), this, s"command")
  val status = Input(Bit(), this, s"status")

  override def connect = {
    /* Control IO */
    prt.cins.foreach { _.ic ==> status }
    prt.couts.foreach { _.ic <== command}
    prt.gouts.foreach { _.valid <== command }
    super.connect
  }
  /* ---------- SIMULATION -----------*/
  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    status.vAt(3)
    command.v.set { v =>
      if (rst) v.setHigh
      else v.setLow
    }
    super.register
  }
}

class MCCtrlBox(param:CtrlBoxParam)(implicit spade:Spade) extends CtrlBox(param) {
  override lazy val prt:MemoryController = collectUp[MemoryController](this).head

  val rdone = Output(Bit(), this, s"${this}.rdone")
  val wdone = Output(Bit(), this, s"${this}.wdone")
  val en = Module(Delay(Bit(), 0, s"$prt.en"))
  val WAITING = false
  val RUNNING = true
  val state = Output(Bit(), this, s"${this}.state")
  val running = Output(Bit(), this, s"${this}.running")
  val count = Output(Word(), this, s"${this}.count")

  override def connect = {
    super.connect
    prt.fifos.foreach { fifo => 
      fifo.dequeueEnable <== en.out
      fifo.enqueueEnable <== fifo.writePortMux.valid 
      fifo.predicate <== spade.top.low.out
    }
    prt.vdata.dequeueEnable <== running
    prt.sdata.dequeueEnable <== running
    prt.fifos.foreach { fifo => fifo.predicate <== spade.top.low.out }

    /* Control IO */
    prt.couts.foreach { cout =>
      cout.ic <== prt.fifos.map(_.notFull)
      cout.ic <== rdone
      cout.ic <== wdone
    }
    prt.gouts.foreach { _.valid <== running }
  }

  /* ---------- SIMULATION -----------*/
  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    import spademeta._
    cfmap.get(prt).foreach { config => 
      state.v.default = WAITING 
      running.v.default = false
      config.mctpe match {
        case tp if tp.isDense =>
          val (done, size) = tp match {
            case TileLoad => (rdone, prt.rsize)
            case TileStore => (wdone, prt.wsize)
            case _ => throw new Exception(s"Not possible match")
          }
          running.v := (state.v =:= RUNNING)
          en.in.v := fifoAndTree.out.v & (done.pv | running.pv.not)
          state.v.set { statev =>
            If(done.v) {
              statev <<= WAITING
            }
            If(en.out.v) {
              statev <<= RUNNING
            }
          }
          val par = spade.numLanes //TODO loader's / store's par
          count.v.set { countv =>
            Match(
              sim.rst -> { () => countv <<= 0 },
              done.pv -> { () => countv <<= 0 },
              (running.pv) -> { () => countv <<= countv + par }
            ) {}
          }
          done.v := running.v & (count.v >= eval(FixSub, size.readPort.v / 4, par))
        case Gather =>
        case Scatter =>
      }
    }
    super.register
  }
}
