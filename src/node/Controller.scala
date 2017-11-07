package spade.node

import pirc.enums._
import pirc.util._
import pirc.exceptions._

import spade._
import spade.util._

trait ControllerParam extends SpadeParam {
  val cfifoSize:Int
  val sfifoSize:Int
  val vfifoSize:Int
  val sramSize:Int
  val numVectorFifos:Int
  val numScalarFifos:Int
  val numControlFifos:Int
  val numSRAMs:Int
  val muxSize:Int
}

class ControllerConfig (val name:String) extends Configuration
/* Controller */
abstract class Controller(val param:ControllerParam)(implicit spade:Spade) extends Routable with Configurable {

  type CT <: ControllerConfig
  import spademeta._
  import param._

  val scalarIO:ScalarIO[this.type] = ScalarIO(this)
  val vectorIO:VectorIO[this.type] = VectorIO(this)
  val ctrlIO:ControlIO[this.type] = ControlIO(this)

  var vfifos:List[VectorMem] = Nil
  val sfifos:List[ScalarMem] = List.tabulate(numScalarFifos)  { i => Module(ScalarMem(sfifoSize)).index(i) }
  val cfifos:List[ControlMem] = List.tabulate(numControlFifos)  { i => Module(ControlMem(cfifoSize)).index(i) }
  val srams:List[SRAM] = List.tabulate(numSRAMs) { i => Module(SRAM(sramSize, spade.numLanes)).index(i) }
  lazy val mems:List[OnChipMem] = vfifos ++ sfifos ++ cfifos ++ srams 
  lazy val fifos:List[LocalMem] = vfifos ++ sfifos ++ cfifos

  def ctrlBox:CtrlBox

  override def connect:Unit = {
    super.connect
    warn(cins.size < numControlFifos, s"${quote(this)} cins=${cins.size} numControlFifos=${numControlFifos}")
    warn(sins.size < numScalarFifos, s"${quote(this)} sins=${sins.size} numScalarFifos=${numScalarFifos}")
    warn(vins.size < numVectorFifos, s"${quote(this)} vins=${vins.size} numVectorFifos=${numVectorFifos}")
    vfifos = List.tabulate(vins.size) { i => Module(VectorMem(vfifoSize)).index(i) }
    fifos.foreach(_.writePortMux.addInputs(muxSize))
    srams.foreach(_.writePortMux.addInputs(1))
    srams.foreach(_.writeAddrMux.addInputs(1))
    srams.foreach(_.readAddrMux.addInputs(1))
    connectInputs
  }

  def connectInputs = {
    // Xbar
    cins.foreach { cin => 
      cfifos.foreach { fifo => 
        fifo.writePortMux.inputs.foreach { _ <== cin.ic }
        fifo.writePortMux.valids.foreach { _ <== cin.valid }
      } 
    }
    sins.foreach { sin => 
      sfifos.foreach { fifo => 
        fifo.writePortMux.inputs.foreach { _ <== sin.ic }
        fifo.writePortMux.valids.foreach { _ <== sin.valid }
      } 
    }
    // One to one
    (vins, vfifos).zipped.foreach { case (vi, fifo) => 
      fifo.writePortMux.inputs.foreach { _ <== vi.ic }
      fifo.writePortMux.valids.foreach { _ <== vi.valid }
    }
  }
}
