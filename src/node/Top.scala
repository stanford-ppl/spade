package spade.node

import spade._
import spade.util._
import spade.network._

import pirc.util._

case class PreloadTopParam (
  override val numArgIns:Int = 6,
  override val numArgOuts:Int = 5,
  override val numRows:Int = 2,
  override val numCols:Int = 2,
  override val pattern:Pattern = MixAll
) extends TopParam () with PreLoadSpadeParam

class TopParam (
  val numArgIns:Int = 6,
  val numArgOuts:Int = 5,
  val numRows:Int = 2,
  val numCols:Int = 2,
  val pattern:Pattern = MixAll,
  val sbufSize:Int = 16,
  val vbufSize:Int = 16
) extends ControllerParam {
  val cbufSize:Int = 0
}

case class TopConfig (
  override val name:String,
  bounds:Map[GlobalOutput[_<:PortType, Module], Option[AnyVal]],
  override val outputValid:Map[GlobalOutput[_<:PortType, Module], Output[_<:PortType, Module]]
) extends ControllerConfig(name, outputValid)

/* Top-level controller (host)
 * @param argIns argument inputs. scalar inputs to the accelerator
 * @param argOuts argument outputs. scalar outputs to the accelerator
 * @param argInBuses output buses argIns are mapped to
 * @param argOutBuses input buses argOuts are mapped to
 * */
case class Top(override val param:TopParam=new TopParam())(implicit spade:Spade) extends Controller(param) with Configurable {

  type CT = TopConfig
  import param._
  import spademeta._

  /* --- Controller at specific coordinate --- */
  def cuAt(x:Int, y:Int) = param.pattern.cuAt(this)(x,y)

  def pcuAt(x:Int, y:Int):PatternComputeUnit = new PatternComputeUnit(spade.pcuAt(x,y)).coord(x,y)

  def mcuAt(x:Int, y:Int):MemoryComputeUnit = new MemoryComputeUnit(spade.mcuAt(x,y)).coord(x,y)

  def scuAt(x:Int, y:Int):ScalarComputeUnit = new ScalarComputeUnit(spade.scuAt(x,y)).coord(x,y)

  def mcAt(x:Int, y:Int):MemoryController = new MemoryController(spade.mcAt(x,y)).coord(x,y)

  def ocuAt(x:Int, y:Int):OuterComputeUnit = new OuterComputeUnit(spade.ocuAt(x,y)).coord(x,y)

  /* --- Submodule Instantiation --- */

  lazy val ctrlBox:TopCtrlBox = TopCtrlBox()

  val dram = DRAM(size=1024) 

  val cuArray:List[List[Controller]] = List.tabulate(numCols, numRows) { case (x, y) => cuAt(x, y) }

  val dramAGs = List.tabulate(2, numRows+1) { case (x, y) => if (x==0) scuAt(-1, y) else scuAt(numCols, y) }

  val sramAGs = List.tabulate(2, numRows+1) { case (x, y) => if (x==0) pcuAt(-1, y) else pcuAt(numCols, y) }

  val mcArray = List.tabulate(2, numRows+1) { case (x, y) => if (x==0) mcAt(-1, y) else mcAt(numCols, y) }

  val sbArray:List[List[SwitchBox]] = List.tabulate(numCols+1, numRows+1) { case (x, y) => SwitchBox().coord(x,y) }

  val ocuArray = List.tabulate(numCols+1, numRows+1) { case (x, y) => ocuAt(x, y) }

  /* --- All Controllers --- */
  val prts = this :: 
                cuArray.flatten ++ 
                dramAGs.flatten ++ 
                sramAGs.flatten ++ 
                mcArray.flatten ++ 
                sbArray.flatten ++ 
                ocuArray.flatten

  val sbs:List[SwitchBox] = sbArray.flatten

  lazy val ctrlers:List[Controller]      = prts.collect    { case cl:Controller          => cl }
  lazy val cus:List[ComputeUnit]         = ctrlers.collect { case cu:ComputeUnit         => cu }
  lazy val pcus:List[PatternComputeUnit] = ctrlers.collect { case pcu:PatternComputeUnit => pcu }
  lazy val mcus:List[MemoryComputeUnit]  = ctrlers.collect { case mcu:MemoryComputeUnit  => mcu }
  lazy val scus:List[ScalarComputeUnit]  = ctrlers.collect { case scu:ScalarComputeUnit  => scu }
  lazy val ocus:List[OuterComputeUnit]   = ctrlers.collect { case ocu:OuterComputeUnit   => ocu }
  lazy val mcs:List[MemoryController]    = ctrlers.collect { case mc:MemoryController    => mc }

  /* --- Network --- */

  lazy val scalarNetwork = new ScalarNetwork()
  lazy val vectorNetwork = new VectorNetwork()
  lazy val ctrlNetwork = new CtrlNetwork()

  /* --- properties -- */
  def diameter = Math.max(
                  numRows + numCols, // Allow top left to talk to top right
                  Math.ceil(numRows*1.0/2).toInt+3 // allow top to talk to middle CUs
                )
  def numCUs = cuArray.size

  override def register(implicit sim:Simulator):Unit = {
    import sim.util._
    val config = cfmap(this)
    val bounds = config.bounds
    souts.foreach { psout =>
      bounds.get(psout).foreach { _ match {
        case Some(b:Int) => 
          psout.ic.v.asSingle := b
        case Some(b:Float) => 
          psout.ic.v.asSingle := b
        case None => warn(s"$psout doesn't have a bound")
        case b => err(s"Don't know how to simulate bound:$b of $psout")
      } }
    }
    super.register
  }

  def config:Unit = {
    scalarNetwork.reset
    ctrlNetwork.reset
    vectorNetwork.reset
    scalarNetwork.config
    ctrlNetwork.config
    vectorNetwork.config

    this.genConnections
    sbs.foreach { _.genConnections }
    pcus.foreach { _.config }
    mcus.foreach { _.config }
    //mus.foreach { _.config }
    scus.foreach { _.config }
    mcs.foreach { _.config }
    ocus.foreach { _.config }
  }
}
