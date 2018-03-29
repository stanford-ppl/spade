package spade.node
import prism.enums._

sealed trait PinType extends Enum
trait Bit extends PinType
trait Word extends PinType
trait Vector extends PinType
case object Bit extends PinType
case object Word extends PinType
case object Vector extends PinType

trait RegColor extends Enum
case object VecInReg extends RegColor
case object VecOutReg extends RegColor
case object ScalarInReg extends RegColor
case object ScalarOutReg extends RegColor
case object ReadAddrReg extends RegColor
case object WriteAddrReg extends RegColor
case object CounterReg extends RegColor
case object ReduceReg extends RegColor
case object AccumReg extends RegColor

sealed trait SwitchConnection extends Enum
case object CrossBarSwitchConnection extends SwitchConnection
