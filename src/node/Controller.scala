package spade.node

import pirc.enums._
import pirc.util._
import pirc.exceptions._

import spade._
import spade.util._

trait ControllerParam extends SpadeParam {
  val cbufSize:Int
  val sbufSize:Int
  val vbufSize:Int
}

class ControllerConfig (val name:String) extends Configuration
/* Controller */
abstract class Controller(val param:ControllerParam)(implicit spade:Spade) extends Routable with Configurable {

  type CT <: ControllerConfig
  import spademeta._
  import param._

  lazy val scalarIO:ScalarIO[this.type] = ScalarIO(this)
  lazy val vectorIO:VectorIO[this.type] = VectorIO(this)
  lazy val ctrlIO:ControlIO[this.type] = ControlIO(this)

  var vfifos:List[VectorMem] = Nil
  var sfifos:List[ScalarMem] = Nil
  var cfifos:List[ControlMem] = Nil
  def fifos:List[LocalMem] = cfifos ++ sfifos ++ vfifos
  def mems:List[OnChipMem] = cfifos ++ sfifos ++ vfifos
  def numControlBufs(num:Int):this.type = { cfifos = List.tabulate(num)  { i => ControlMem(cbufSize).index(i) }; this }
  def numScalarBufs(num:Int):this.type = { sfifos = List.tabulate(num)  { i => ScalarMem(sbufSize).index(i) }; this }
  def numScalarBufs:Int = sfifos.size
  def numVecBufs(num:Int):this.type = { vfifos = List.tabulate(num) { i => VectorMem(vbufSize).index(i) }; this }
  def numVecBufs:Int = vfifos.size

  def ctrlBox:CtrlBox
  def config:Unit
}
