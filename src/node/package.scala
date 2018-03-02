package spade

import spade.params._
import spade.network._
import prism._

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

  def naming[T<:SpadeNode](n:T, s:String)(implicit design:SpadeDesign) = {
    import design.spademeta._
    nameOf(n) = s
    n
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
