package spade.codegen 

import spade._
import spade.node._

import pirc._
import pirc.util._
import pirc.enums._
import pirc.codegen._

import scala.collection.mutable.ListBuffer

class ConfigCodegen(implicit design: Spade) extends Codegen with ScalaCodegen with MultiFileCodegen {
  def shouldRun = Config.codegen
  var mapping:SpadeMap = _
  def init(mapping:SpadeMap) = this.mapping = mapping
  lazy val maps = mapping // This is hacky but I'm lazy ...
  import maps._
  import design._
  import design.topParam._

  lazy val appName = cfmap(top).name
  lazy val traitName = appName + "Trait"
  lazy val dir = sys.env("PLASTICINE_HOME") + s"/src/main/scala/apps/$design"
  override lazy val stream = newStream(dir, s"$traitName.scala")

  //val SVT = "SrcValueTuple"
  val SVT = "SVT"

  def emitHeader = {
    emitln(s"package plasticine.apps")
    emitln(s"import plasticine.arch._")
    emitln(s"import chisel3._")
    emitln(s"import plasticine.spade._")
    emitln(s"import plasticine.pisa.PISAPIR")
    emitln(s"import plasticine.pisa.ir.{SrcValueTuple => $SVT, _}")
    emitln(s"import chisel3.util._")
    emitln(s"import scala.collection.mutable.ListBuffer")
    emitln(s"import GeneratedTopParams.plasticineParams._")
    emitln(s"import GeneratedTopParams._")
    emitln(s"import plasticine.templates._")
    emitln(s"import plasticine.pisa.enums._")
    emitln(1)
  }

  def emitAppObject {
    emitln(s"object $appName extends PISAPIR with $traitName")
  }

  override def splitPreHeader:Unit = {
    emitHeader
  }

  override def splitPostHeader:Unit = {
    emitln(s"self:$traitName =>")
    emitBSln(s"def config${fileNumber}:Unit =")
  }

  override def splitPreFooter:Unit = {
    emitBEln
  }

  addPass(canRun=true, runCount=1) {
    emitHeader
    emitAppObject
    emitSplit {
      emitCrossbarBits
      emitCUBits
    }
    emitMixed {
      emitPlasticineBits
      emitBlock(s"def config:Unit =") {
        (0 until fileNumber).foreach { i =>
          emitln(s"config${i+1}")
        }
      }
      emitln(s"config")
    }
  }

  def muxIdx(in:Input[_<:PortType, Module]) = {
    fimap.get(in).fold(-1) { out => in.indexOf(out) }
  }
  def muxIdx(out:GlobalOutput[_<:PortType, Module]) = {
    fimap.get(out.ic).fold(-1) { _.src.index }
  }

  def emitXbar(name:String, ins:List[Input[_<:PortType, Module]]) = {
    ins.zipWithIndex.foreach { case (in, idx) =>
      val id = muxIdx(in)
      indexOf.get(in).foreach { i => assert(i == idx) }
      if (id != -1) {
        emitln(s"${name}.outSelect(${idx}) = $id")
      }
    }
  }

  def emitCrossbarBits = {
    sbArray.foreach {
      _.foreach { sb =>
        emitXbar(s"${qv(sb)}", sb.vouts.map(_.ic))
        emitXbar(s"${qs(sb)}", sb.souts.map(_.ic))
        emitXbar(s"${qc(sb)}", sb.couts.map(_.ic))
      }
    }
  }

  //def lookUp(n:I):String = { lookUp(ipmap(n)) }

  def lookUp(n:Input[_<:PortType,Module]):String = fimap.get(n) match {
    case None => s"$SVT()"
    case Some(o) =>
      val (src, value) = o.propogate.src match {
        case s:Counter => ("CounterSrc", s.index)
        case s:ScalarMem => ("ScalarFIFOSrc", s.index)
        case s:VectorMem => ("VectorFIFOSrc", s.index)
        case s:Const[_] =>
          val const = cfmap(s).value
          ("ConstSrc", const)
        case s:PipeReg =>
          n.src match {
            case ts:PipeReg if ts.stage.isNext(s.stage) => ("PrevStageSrc", s.reg.index)
            case ts:PipeReg if ts.stage == s.stage => ("CurrStageSrc", s.reg.index)
            case fu:FuncUnit if cfmap(fu.stage).isReduce => ("ReduceTreeSrc", s.reg.index)
            case fu:FuncUnit if fu.stage.isNext(s.stage) => ("PrevStageSrc", s.reg.index)
            case fu:FuncUnit if fu.stage == s.stage => ("CurrStageSrc", s.reg.index)
            case ts:PipeReg => throw new Exception(s"toStage=${quote(ts.stage)} prev=${ts.stage.prev.map(quote).getOrElse("None")} currStage=${quote(s.stage)}")
          }
        case s:FuncUnit => ("ALUSrc", s.stage.index)
      }
      s"$SVT($src, $value)"
  }

  def lookUp(n:Output[_<:PortType,Module]):List[String] = {
    fimap(n).toList.map { i =>
      val (src,value) =  i.propogate.src match {
        case s:PipeReg =>
          n.src match {
            case ts:FuncUnit => ("CurrStageDst", s"${s.reg.index}")
          }
        case s:GlobalOutput[_,_] if networkOf(s).isVectorNetwork => ("VectorOutDst", s.index)
        case s:GlobalOutput[_,_] if networkOf(s).isScalarNetwork => ("ScalarOutDst", s.index)
        case s:SRAM if i == s.readAddr || s.readAddr.slices.map(_.in).contains(i) =>
          ("ReadAddrDst", -1)
        case s:SRAM if i == s.writeAddr || s.writeAddr.slices.map(_.in).contains(i) =>
          ("WriteAddrDst", -1)
      }
      s"$SVT($src, $value)"
    }
  }

  def lookUp(n:AndTree):List[Int] = {
    //n.ins.map { in => if (ipmap.contains(in)) 1 else 0 }
    n.ins.map { in => muxIdx(in) }
  }

  def emitCtrBits(pcu:ComputeUnit) = {
    pcu.ctrs.foreach { pctr =>
      cfmap.get(pctr).foreach { config =>
        val ctrBit = s"CounterRCBits(max=${lookUp(pctr.max)}, stride=${lookUp(pctr.step)}, min=${lookUp(pctr.min)}, par=${config.par})"
        emitln(s"${quote(pctr)} = $ctrBit")
      }
    }
  }

  def emitCChainBis(pcu:ComputeUnit) = {
    val pctrs = pcu.ctrs
    val chain = List.tabulate(pctrs.size-1) { i =>
      if (cfmap.contains(pctrs(i)) && cfmap.contains(pctrs(i+1))) {
        if (fimap.get(pctrs(i).en) == Some(pctrs(i+1).done)) 1 else 0
      } else 0
    }
    //emitln(s"val ${q(pcu, "ctrs")} = Array.tabulate(${pcu.ctrs.size}) { i => CounterRCBits.zeroes(${spade.wordWidth})}")
    //emitln(s"val ${q(pcu, "cc")} = CounterChainBits(${quote(chain)}, ${q(pcu, "ctrs")})")
    //emitln(s"${quote(pcu)}.counterChain = CounterChainBits(${quote(chain)}, ${q(pcu, "ctrs")})")
    emitln(s"${quote(pcu)}.counterChain.chain = ${quote(chain)}")
  }

  def emitFwdRegs(pst:Stage) = {
    pst.prs.foreach { pr =>
      if (fimap.contains(pr.in)) {
        emitln(s"${quote(pr)} = ${lookUp(pr.in)}")
      }
    }
  }

  def emitPipeReg(pcu:ComputeUnit, ppr:PipeReg) = {
    cfmap.get(ppr).foreach { config =>
      config.init.foreach { init =>
        emitln(s"//TODO: ${quote(pcu)}.stages(${ppr.stage.index}).reg(${ppr.reg.index}).init = ${init}")
      }
    }
    //import pir.util.collectOut
    //collectOut[PR](fu.out).foreach { pr =>
      //pr.reg match {
        //case AccumPR(Const(init)) => emitln(s"${quote(pcu)}.accumInit = $init")
        //case _ =>
      //}
    //}
  }

  def emitStageBits(pcu:ComputeUnit) = {
    pcu.stages.foreach { pst =>
      cfmap.get(pst).fold {
      } { config =>
        val pfu = pst.funcUnit
        emitln(s"${quote(pst)}.opA = ${lookUp(pfu.operands(0))}")
        emitln(s"${quote(pst)}.opB = ${lookUp(pfu.operands(1))}")
        emitln(s"${quote(pst)}.opC = ${lookUp(pfu.operands(2))}")
        emitln(s"${quote(pst)}.opcode = ${quote(config.op)}")
        assert(fimap(pfu.out).size==1)
        if (fimap(pfu.out).head.isSrcSlice) {
          emitln(s"${quote(pst)}.res = ${quote(lookUp(pfu.out.sliceHead.out))}")
        } else {
          emitln(s"${quote(pst)}.res = ${quote(lookUp(pfu.out))}")
        }
        pst.prs.foreach { ppr => emitPipeReg(pcu, ppr) }
        //cu match {
          //case cu:MP if forWrite(st) =>
            //emitln(s"${quote(pst)}.enableSelect.src = WriteEnSrc")
          //case cu:MP if forRead(st) =>
            //emitln(s"${quote(pst)}.enableSelect.src = ReadEnSrc")
          //case _ =>
        //}
      }
      emitFwdRegs(pst)
    }
  }

  def cuTp(pcu:ComputeUnit) = pcu match {
    case cu:PatternMemoryUnit => "PMU"
    case cu:ComputeUnit => "PCU"
  }

  def emitStreamingMuxSelect(pcu:ComputeUnit) = {
    val pcb = pcu.ctrlBox
    //pcb match {
      //case pcb:InnerCtrlBox =>
        //val cu = pmmap(pcu)
        //emitComment(s"$cu isPipelining=${isPipelining(cu)} isStreaming=${isStreaming(cu)}")
        //emitln(s"${quote(pcb)}.streamingMuxSelect = ${muxIdx(pcb.en.in)}")
      //case pcb =>
    //}
  }

  def commentUDCs(pcu:ComputeUnit) = {
    val names = pcu.ctrlBox.udcs.map { pudc => cfmap.get(pudc).map { _.name } }
    emitComment(s"udcs = ${quote(names)}")
  }

  def commentFIFOs(pcl:Controller) = {
    pcl.fifos.foreach { pfifo =>
      cfmap.get(pfifo).foreach { config =>
        emitComment(s"${quote(pfifo)} -> ${config.name}")
      }
    }
  }

  def emitPulserSM(pcu:ComputeUnit) = {
    //val cu = pmmap(pcu)
    //cu match {
      //case cu:Seq =>
        //emitln(s"${quote(pcu.ctrlBox)}.pulserMax=1")
      //case cu:MetaPipe =>
        //emitln(s"${quote(pcu.ctrlBox)}.pulserMax=${lengthOf(cu)}")
      //case _ =>
    //}
  }

  //def commentIO(pios:List[PGIO[PModule]]) = {
    //pios.foreach { 
      //case pin:PGI[PModule] =>
        //vimap.get(pin).foreach { ins =>
          //emitComment(s"${quote(pin)} -> ${ins.map(in => s"${in}(from:${in.from} at ${in.from.ctrler})").mkString(",")}")
        //}
      //case pout:GlobalOutput[_,Module] =>
        //vomap.get(pout).foreach { out =>
          //emitComment(s"${quote(pout)} -> ${out}(to:${out.to.map{ in => s"$in at ${in.ctrler}"}.mkString(",")})")
        //}
    //}
  //}

  def emitUDCInits(pcu:Controller) = {
    val inits = pcu.ctrlBox.udcs.map { pudc => cfmap.get(pudc).map { _.initVal } }
    if (inits.nonEmpty && inits.exists{_.nonEmpty})
    emitln(s"${quote(pcu.ctrlBox)}.udcInit=${quote(inits.map(_.getOrElse(-1)))}")
  }

  def emitXbars(pcl:Controller) = {
    pcl.ctrlBox match {
      case pcb:InnerCtrlBox =>
        emitXbar(s"${quote(pcb)}.incrementXbar", pcb.udcs.map(_.inc))
        emitXbar(s"${quote(pcb)}.swapWriteXbar", pcl.sfifos.map(_.enqueueEnable))
        emitXbar(s"${quote(pcb)}.tokenOutXbar", pcl.couts.map(_.ic))
        emitXbar(s"${quote(pcb)}.doneXbar", List(pcb.done.in))
      case pcb:OuterCtrlBox =>
        emitXbar(s"${quote(pcb)}.incrementXbar", pcb.udcs.map(_.inc))
        emitln(s"${quote(pcb)}.udcDecSelect=${quote(pcb.udcs.map(udc => muxIdx(udc.dec)))}")
        emitXbar(s"${quote(pcb)}.swapWriteXbar", pcl.sfifos.map(_.enqueueEnable))
        emitXbar(s"${quote(pcb)}.tokenOutXbar", pcl.couts.map(_.ic))
        emitXbar(s"${quote(pcb)}.doneXbar", List(pcb.done.in))
      case pcb:MemoryCtrlBox =>
        emitXbar(s"${quote(pcb)}.swapWriteXbar", pcl.sfifos.map(_.enqueueEnable))
        emitXbar(s"${quote(pcb)}.readDoneXbar", List(pcb.readDone.in))
        emitXbar(s"${quote(pcb)}.writeDoneXbar", List(pcb.writeDone.in))
        emitXbar(s"${quote(pcb)}.tokenOutXbar", pcl.couts.map(_.ic))
      case pcb:MCCtrlBox =>
        emitXbar(s"${quote(pcb)}.tokenInXbar", pcb.prt.sfifos.map(_.enqueueEnable))
        emitXbar(s"${quote(pcb)}.tokenOutXbar", pcl.couts.map(_.ic))
    }
  }

  def emitAndTree(pcb:CtrlBox, at:AndTree) = {
    val config = lookUp(at)
    if (config.nonEmpty) emitln(s"${quote(pcb)}.${at.name.get} = ${config}")
  }

  def emitAndTrees(pcu:ComputeUnit) = {
    val pcb = pcu.ctrlBox
    pcb match {
      case pcb:InnerCtrlBox =>
        emitAndTree(pcb, pcb.tokenInAndTree)
        emitAndTree(pcb, pcb.fifoAndTree)
        emitAndTree(pcb, pcb.siblingAndTree)
      case pcb:OuterCtrlBox =>
        emitAndTree(pcb, pcb.childrenAndTree)
        emitAndTree(pcb, pcb.siblingAndTree)
      case pcb:MemoryCtrlBox =>
        emitAndTree(pcb, pcb.writeFifoAndTree)
        emitAndTree(pcb, pcb.readFifoAndTree)
      case pcb:TopCtrlBox =>
      case pcb:CtrlBox =>
    }
  }

  def emitSwapReadSelect(pcu:ComputeUnit) = {
    val pcb = pcu.ctrlBox
    pcu match {
      case pcu:PatternMemoryUnit =>
        val idxes = pcu.sfifos.map(sbuf => muxIdx(sbuf.dequeueEnable))
        emitln(s"${quote(pcb)}.scalarSwapReadSelect = ${quote(idxes)}")
      case pcu =>
    }
  }

  def emitControlBits(pcu:ComputeUnit) = {
    val pcb = pcu.ctrlBox
    //commentIO(pcu.cins)
    //commentIO(pcu.couts)
    commentUDCs(pcu)
    emitUDCInits(pcu)
    emitAndTrees(pcu)
    emitStreamingMuxSelect(pcu)
    commentFIFOs(pcu)
    emitXbars(pcu)
    emitPulserSM(pcu)
    emitSwapReadSelect(pcu)
  }

  def emitControlBits(pmc:MemoryController) = {
    val pcb = pmc.ctrlBox
    commentFIFOs(pmc)
    emitXbars(pmc)
  }

  def emitScalarInXbar(pcl:Controller) = {
    val sins = pcl.sfifos.map { sbuf => fimap.get(sbuf.writePort).map { po => po.src } }
    emitComment(s"${quote(pcl)}.scalarInXbar=[${sins.mkString(",")}]")
    emitXbar(s"${quote(pcl)}.scalarInXbar", pcl.sfifos.map(_.writePort))
  }

  def emitScalarOutXbar(pcu:ComputeUnit) = {
    val souts = pcu.souts.map { sout => fimap.get(sout.ic).map { po => po.propogate.src } }
    emitComment(s"${quote(pcu)}.scalarOutXbar=[${souts.mkString(",")}]")
    val soRegs = pcu.regs.filter{ _.is(ScalarOutReg) }
    val soIdxes = souts.map(_.map(ppr => soRegs.indexOf(ppr.asInstanceOf[PipeReg].reg) ).getOrElse(-1))
    emitXbar(s"${quote(pcu)}.scalarOutXbar", pcu.souts.map(_.ic))
  }

  def emitSRAM(psram:SRAM) = {
    val pcu = psram.prt
    cfmap.get(psram).foreach { config =>
      emitComment(s"$psram -> ${config.name}")
      val stride = config.banking match {
        case Strided(stride, banks) => stride
        case _ => -1 //TODO
      }
      emitln(s"${quote(psram)}.stride = $stride")
      emitln(s"${quote(psram)}.numBufs = ${config.bufferSize}")
      //TODO: handle config for multiple selections
      emitln(s"${quote(psram.prt)}.wdataSelect = ${lookUp(psram.writePortMux.inputs.head)}")
      emitln(s"${quote(psram.prt)}.waddrSelect = ${lookUp(psram.writeAddrMux.inputs.head)}")
      emitln(s"${quote(psram.prt)}.raddrSelect = ${lookUp(psram.readAddrMux.inputs.head)}")
    }
  }

  def emitScratchpadBits(pcu:ComputeUnit) = {
    pcu match {
      case pcu:PatternMemoryUnit => emitSRAM(pcu.sram)
      case _ =>
    }
  }

  def emitScalarNBuffer(pcu:ComputeUnit) = {
    val nbufs = pcu.sfifos.map { psbuf =>
      cfmap.get(psbuf).fold(-1) { _.bufferSize }
    }
    emitln(s"${quote(pcu)}.fifoNbufConfig=${quote(nbufs)}")
  }

  def emitCUBit(pcu:Controller) = {
    cfmap.get(pcu).foreach { config =>
      emitComment(s"Configuring ${quote(pcu)} <- ${config.name}")
      pcu match {
        case pcu:OuterComputeUnit =>
          emitCChainBis(pcu)
          emitControlBits(pcu)
          emitCtrBits(pcu)
        case pcu:ComputeUnit =>
          emitControlBits(pcu)
          emitScalarNBuffer(pcu)
          emitScalarInXbar(pcu)
          emitScalarOutXbar(pcu)
          emitCChainBis(pcu)
          emitCtrBits(pcu)
          emitStageBits(pcu)
          emitScratchpadBits(pcu)
      }
    }
  }

  def commentMC(pmc:MemoryController) = {
    emitComment(s"mctpe=${cfmap(pmc).mctpe}")
  }

  def emitMCBit(pmc:MemoryController) = {
    cfmap.get(pmc).foreach { config =>
      emitComment(s"Configuring ${quote(pmc)} <- $config")
      commentMC(pmc)
      emitScalarInXbar(pmc)
      emitControlBits(pmc)
    }
  }

  def emitCUBits = {
    cuArray.foreach {
      _.foreach { cu => emitCUBit(cu) }
    }
    ocuArray.foreach {
      _.foreach { cu => emitCUBit(cu) }
    }
    dramAGs.foreach {
      _.foreach { cu => emitCUBit(cu) }
    }
    mcArray.foreach {
      _.foreach { cu => emitMCBit(cu) }
    }
  }

  def emitMain {
    emitln("def main(args: String*) = plasticineBits")
  }

  //def commentArgIns = {
    //top.souts.foreach { psout =>
      //vomap.get(psout).foreach { sout =>
        //emitComment(s"${quote(psout)} -> $sout")
      //}
    //}
  //}

  def emitPlasticineBits = {
    emitLambda(s"val cuArray:Array[Array[CUBits]] = Array.tabulate(${cuArray.size}, ${cuArray.head.size})", "case (i,j)") {
      emitBlock(s"cuParams(i)(j) match") {
        emitln("case p:PCUParams => PCUBits.zeroes(p)")
        emitln("case p:PMUParams => PMUBits.zeroes(p)")
      }
    }
    emitLambda(s"val csbs = Array.tabulate(${sbArray.size}, ${sbArray.head.size})", "case (i,j)") {
      emitln(s"CrossbarBits.zeroes(controlSwitchParams(i)(j))")
    }
    emitLambda(s"val ssbs = Array.tabulate(${sbArray.size}, ${sbArray.head.size})", "case (i,j)") {
      emitln(s"CrossbarBits.zeroes(scalarSwitchParams(i)(j))")
    }
    emitLambda(s"val vsbs = Array.tabulate(${sbArray.size}, ${sbArray.head.size})", "case (i,j)") {
      emitln(s"CrossbarBits.zeroes(vectorSwitchParams(i)(j))")
    }
    emitLambda(s"val lcus = Array.tabulate(${ocuArray.size}, ${ocuArray.head.size})", "case (i,j)") {
      emitln(s"SwitchCUBits.zeroes(switchCUParams(i)(j))")
    }
    emitLambda(s"val dags = Array.tabulate(${dramAGs.size}, ${dramAGs.head.size})", "case (i,j)") {
      emitln(s"ScalarCUBits.zeroes(scalarCUParams(i)(j))")
    }
    emitLambda(s"val mcs = Array.tabulate(${mcArray.size}, ${mcArray.head.size})", "case (i,j)") {
      emitln(s"MemoryChannelBits.zeroes(memoryChannelParams(i)(j))")
    }

    implicit val ms = new CollectionStatus(false)

    emitInst(s"val plasticineBits = PlasticineBits") { implicit ms:CollectionStatus =>
      emitComma(s"cu=cus")
      emitComma(s"vectorSwitch=vsbs")
      emitComma(s"scalarSwitch=ssbs")
      emitComma(s"controlSwitch=csbs")
      emitComma(s"switchCU=lcus")
      emitComma(s"scalarCU=dags")
      emitComma(s"memoryChannel=mcs")
      emitComma(s"argOutMuxSelect=${quote(top.sins.map { in => muxIdx(in) })}")
      assert(top.cins.size==1)
      emitComma(s"doneSelect=${muxIdx(top.cins.head)}")
    }("")
    //commentArgIns
    emitMain
  }

  def quote(n:Node):String = n match {
    //case n:EmptyStage => s"EmptyStage"
    //case n:WAStage => s"WAStage(numOprds=${n.fu.numOprds}, ops=${quote(n.fu.ops)})"
    //case n:RAStage => s"RAStage(numOprds=${n.fu.numOprds}, ops=${quote(n.fu.ops)})"
    //case n:FUStage => s"FUStage(numOprds=${n.fu.numOprds}, ops=${quote(n.fu.ops)})"
    case n:ScalarComputeUnit =>
      val (x, y) = coordOf(n)
      x match {
        case -1 => s"dags(0)($y)"
        case `numCols` => s"dags(1)($y)"
        case _ => s"cus($x)($y).asSCUBits"
      }
    case n:MemoryController =>
      val (x, y) = coordOf(n)
      x match {
        case -1 => s"mcs(0)($y)"
        case `numCols` => s"mcs(1)($y)"
      }
    case n:OuterComputeUnit =>
      val (x, y) = coordOf(n)
      s"lcus($x)($y)"
    case n:PatternMemoryUnit =>
      val (x, y) = coordOf(n)
      s"cus($x)($y).asPMUBits"
    case n:ComputeUnit =>
      val (x, y) = coordOf(n)
      s"cus($x)($y).asPCUBits"
    case n:Stage =>
      s"${quote(n.prt)}.stages(${n.index})"
    case n:Counter =>
      s"${quote(n.prt)}.counterChain.counters(${n.index})"
    case n:PipeReg =>
      val pcu = n.prt
      val pst = n.stage
      s"${quote(pst)}.fwd(${n.reg.index})"
    case n:MCCtrlBox =>
      s"${quote(n.prt)}"
    case n:CtrlBox =>
      s"${quote(n.prt)}.control"
    case n:SRAM =>
      s"${quote(n.prt)}.scratchpad"
    case n => super.quote(n)
  }

  def quote(n:Op) = n match {
    case Bypass => s"BypassA"
    case n => s"$n"
  }

  def quote(n:List[_]):String = s"List(${n.mkString(",")})"

  def qv(n:Any):String = n match {
    case n:SwitchBox =>
      val (x, y) = coordOf(n)
      s"vsbs($x)($y)"
    case n => quote(n)
  }

  def qs(n:Any):String = n match {
    case n:SwitchBox =>
      val (x, y) = coordOf(n)
      s"ssbs($x)($y)"
    case n => quote(n)
  }

  def qc(n:Any):String = n match {
    case n:SwitchBox =>
      val (x, y) = coordOf(n)
      s"csbs($x)($y)"
    case n => quote(n)
  }

  def q(n:ComputeUnit, pm:String):String = {
    val (x, y) = coordOf(n)
    s"${pm}_${x}_${y}"
  }

}
