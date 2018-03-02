package spade

import spade.params._

package object node {

  private[node] type Design = SpadeDesign

  def indexing[T<:SpadeNode](ns:List[T])(implicit design:SpadeDesign):List[T] = {
    import design.spademeta._
    ns.zipWithIndex.foreach { case (n, i) =>
      indexOf(n) = i
    }
    ns
  }

  def isPCU(n:Any) = n match {
    case cu:CU => cu.param.isInstanceOf[PCUParam]
    case _ => false
  }

  def isPMU(n:Any) = n match {
    case cu:CU => cu.param.isInstanceOf[PMUParam]
    case _ => false
  }

  def isSCU(n:Any) = n match {
    case cu:CU => cu.param.isInstanceOf[SCUParam]
    case _ => false
  }
}
