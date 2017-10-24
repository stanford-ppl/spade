package spade.node

import spade._
import spade.util._

import pirc.enums._
import pirc.util._
import pirc.exceptions._

case class MemoryControllerParam (
  cfifoSize:Int = 16,
  sfifoSize:Int = 16,
  vfifoSize:Int = 16
) extends ControllerParam {
  override val numScalarFifos = 5
  override val numVectorFifos = 1
  override val numControlFifos = 0
  override val numSRAMs = 0
  override val sramSize = 0
  override val muxSize = 1
}

case class MemoryControllerConfig (
  override val name:String,
  mctpe:MCType
) extends ControllerConfig(name)

class MemoryController(param:MemoryControllerParam)(implicit spade:Spade) extends Controller(param) 
  with Configurable {
  import spademeta._
  import param._
  type CT = MemoryControllerConfig

  override val typeStr = "mc"
  val ctrlBox:MCCtrlBox = Module(new MCCtrlBox(CtrlBoxParam()))

  lazy val woffset = sfifos.filter{ sb => nameOf(sb)=="woffset" }.head
  lazy val roffset = sfifos.filter{ sb => nameOf(sb)=="roffset" }.head
  lazy val wsize = sfifos.filter{ sb => nameOf(sb)=="wsize" }.head
  lazy val rsize = sfifos.filter{ sb => nameOf(sb)=="rsize" }.head
  lazy val sdata = sfifos.filter{ vb => nameOf(vb)=="sdata" }.head
  lazy val vdata = vfifos.filter{ vb => nameOf(vb)=="vdata" }.head
  /* Parameters */

  override def connect:Unit = {
    super.connect
    nameOf(sfifos(0)) = "roffset"
    nameOf(sfifos(1)) = "woffset"
    nameOf(sfifos(2)) = "rsize"
    nameOf(sfifos(3)) = "wsize"
    nameOf(sfifos(4)) = "sdata"
    nameOf(vfifos(0)) = "vdata"
  }

  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    val dram = spade.top.dram
    cfmap.get(this).foreach { config => 
      config.mctpe match {
        case TileLoad =>
          vouts.foreach { vout =>
            vout.ic.v.set { v =>
              If (ctrlBox.running.v) {
                val so = roffset.readPort.v.toInt / 4
                val sz = rsize.readPort.v.toInt / 4
                dprintln(s"${quote(this)} TileLoad roffset=$so rsize=$sz ${ctrlBox.count.v.update}")
                dram.updateMemory
                v.foreach { case (ev, i) =>
                  ev <<= dram.memory(so + i + ctrlBox.count.v.toInt)
                }
              }
              //v.valid <<= ctrlBox.running.v //TODO
            }
          }
        case TileStore =>
          dram.setMem { memory =>
            If (ctrlBox.running.v) {
              vdata.readPort.v.foreach { case (ev, i) =>
                val so = woffset.readPort.v.toInt / 4
                val sz = wsize.readPort.v.toInt / 4
                dprintln(s"${quote(this)} TileStore woffset=$so wsize=$sz ${ctrlBox.count.v.update}")
                memory(so + i + ctrlBox.count.v.toInt) <<= ev
              }
            }
          }
        case Gather =>
        case Scatter =>
      }
    }
    super.register
  }
}
