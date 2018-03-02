package spade

import spade.params._
import spade.network._
import prism._
import scala.reflect.{ClassTag, classTag}

import scala.language.higherKinds

package object node extends SpadeEnums {

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

  def bctOf(x:Any):ClassTag[_] = x match {
    case x:DirectedEdge[_,_] => x.bct
    case x:Bundle[_] => x.bct
    case x:NetworkBundle[_] => x.bct
    case x:MeshNetwork[_] => x.bct
    case x => throw PIRException(s"don't have ClassTag[_<:BundleType] for $x")
  }

  def is[B<:BundleType:ClassTag](x:Any) = implicitly[ClassTag[B]] == bctOf(x)

  def as[B<:BundleType:ClassTag,A[_<:BundleType]](x:A[_]) = if (is[B](x)) Some(x.asInstanceOf[A[B]]) else None
}
