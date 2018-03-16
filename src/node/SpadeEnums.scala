package spade

import prism.enums._
trait SpadeEnums {
  sealed trait BundleType extends Enum
  trait Bit extends BundleType
  trait Word extends BundleType
  trait Vector extends BundleType

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
}
