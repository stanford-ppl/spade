package spade.node

import spade._
import spade.util._

import pirc.enums._
import pirc.util._
import pirc.exceptions._

case class MemoryControllerParam (
  cbufSize:Int = 16,
  sbufSize:Int = 16,
  vbufSize:Int = 16,
  muxSize:Int = 10
) extends ControllerParam {
  def config(mc:MemoryController)(implicit spade:Spade) = {
    import mc.spademeta._
    //assert(sins.size==2)
    //assert(vins.size==1)
    mc.numScalarBufs(5)
    mc.numVecBufs(mc.vins.size)
    mc.mems.foreach(_.writePortMux.addInputs(muxSize))
    nameOf(mc.sfifos(0)) = "roffset"
    nameOf(mc.sfifos(1)) = "woffset"
    nameOf(mc.sfifos(2)) = "rsize"
    nameOf(mc.sfifos(3)) = "wsize"
    nameOf(mc.sfifos(4)) = "sdata"
    nameOf(mc.vfifos(0)) = "vdata"
    mc.genConnections
  }
}

case class MemoryControllerConfig (
  override val name:String,
  mctpe:MCType
) extends ControllerConfig(name, Map.empty) // No need to configure output valid

class MemoryController(param:MemoryControllerParam)(implicit spade:Spade) extends Controller(param) 
  with Configurable {
  import spademeta._
  import param._
  type CT = MemoryControllerConfig

  override val typeStr = "mc"
  lazy val ctrlBox:MCCtrlBox = new MCCtrlBox()

  lazy val woffset = sfifos.filter{ sb => nameOf(sb)=="woffset" }.head
  lazy val roffset = sfifos.filter{ sb => nameOf(sb)=="roffset" }.head
  lazy val wsize = sfifos.filter{ sb => nameOf(sb)=="wsize" }.head
  lazy val rsize = sfifos.filter{ sb => nameOf(sb)=="rsize" }.head
  lazy val sdata = sfifos.filter{ vb => nameOf(vb)=="sdata" }.head
  lazy val vdata = vfifos.filter{ vb => nameOf(vb)=="vdata" }.head
  /* Parameters */
  override def config = param.config(this)

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
