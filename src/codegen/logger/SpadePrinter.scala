package spade.codegen

import spade._
import spade.node._
import spade.traversal._

import pirc._
import pirc.util._

class SpadePrinter(implicit design: Spade) extends Codegen with HiearchicalTraversal {

  def shouldRun = Config.debug

  override lazy val stream = newStream(SpadeConfig.spadeFile)
  
  override def traverseDown(node:Any):Unit = {
    node match {
      case n:Input[_,_] => emitln(n.ms)
      case n:Output[_,_] => emitln(n.mt)
      case n:Module => emitBlock(s"$n") { 
        emitln(s"parent=${n.parent}")
        super.traverseDown(n)
      }
    }
  }

  addPass {
    traverseDown(design.top)
  }

  def emitIO(prt:GridIO[_<:PortType, _<:Routable]):Unit = {
    emitBlock(s"ins") {
      prt.ins.foreach { in =>
        emitln(s"${in.ms}")
      }
    }
    emitBlock(s"outs: ") {
      prt.outs.foreach { out =>
        emitln(s"${out.mt}")
      }
    }
  }

  def emitIO(prt:Routable):Unit = {
    emitBlock(s"${quote(prt)}.vectorIO") { emitIO(prt.vectorIO) } 
    emitBlock(s"${quote(prt)}.scalarIO") { emitIO(prt.scalarIO) } 
    emitBlock(s"${quote(prt)}.ctrlIO") { emitIO(prt.ctrlIO) } 
  }


  def emitCtrlBox(cb:CtrlBox) = cb match {
    case cb:InnerCtrlBox =>
      emitModule(cb.tokenInAndTree)
      emitModule(cb.siblingAndTree)
      emitModule(cb.fifoAndTree)
      cb.udcs.foreach { udc => emitModule(udc) }
      emitModule(cb.en)
      emitModule(cb.done)
    case cb:OuterCtrlBox =>
      emitModule(cb.childrenAndTree)
      emitModule(cb.siblingAndTree)
      emitModule(cb.udsm)
      cb.udcs.foreach { udc => emitModule(udc) }
      emitModule(cb.en)
      emitModule(cb.done)
    case cb:MemoryCtrlBox =>
      emitModule(cb.writeFifoAndTree)
      emitModule(cb.readFifoAndTree)
      emitModule(cb.readEn)
      emitModule(cb.writeEn)
      emitModule(cb.readDone)
      emitModule(cb.writeDone)
    case cb:TopCtrlBox =>
    case cb:CtrlBox =>
  }

  def emitModule(m:Module, block: =>Any = {}) = {
    var name=s"$m"
    m match {
      case m:AndTree => name += s"(${m.name})"
      case _ =>
    }
    emitBlock(name) {
      m.ins.foreach { in =>
        emitln(s"${in.ms}")
      }
      m.outs.foreach { out =>
        emitln(s"${out.mt}")
      }
      block
    }
  }

  override def finPass = {
    close
    endInfo(s"Finishing Spade Config Printing in ${getPath}")
  }

}
