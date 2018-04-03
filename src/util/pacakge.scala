package spade

package object util {
  type Spade = spade.Spade
  type SpadeDesign = spade.node.SpadeDesign
  type SpadeNode = spade.node.SpadeNode
  type PinType = spade.node.PinType
  type Pin[P<:PinType] = spade.node.Pin[P]
  type Module = spade.node.Module
  type SpadePass = spade.pass.SpadePass
  type SpadeWorld = spade.pass.SpadeWorld
  type SpadeTraversal = spade.pass.SpadeTraversal
  type SpadeMapLike = spade.config.SpadeMapLike

  def zip[T1, T2, T](x1:Option[T1], x2:Option[T2])(lambda:(T1,T2) => T):Option[T] = (x1, x2) match {
    case (Some(x1), Some(x2)) => Some(lambda(x1, x2))
    case _ => None
  }
  def zip[T1, T2, T3, T](x1:Option[T1], x2:Option[T2], x3:Option[T3])(lambda:(T1,T2,T3) => T):Option[T] = (x1, x2, x3) match {
    case (Some(x1), Some(x2), Some(x3)) => Some(lambda(x1, x2, x3))
    case _ => None
  }
  def zip[T1, T2, T3, T4, T](x1:Option[T1], x2:Option[T2], x3:Option[T3], x4:Option[T4])(lambda:(T1,T2,T3,T4) => T):Option[T] = (x1, x2, x3, x4) match {
    case (Some(x1), Some(x2), Some(x3), Some(x4)) => Some(lambda(x1, x2, x3, x4))
    case _ => None
  }

}
