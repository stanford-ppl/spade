package spade

import spade.params._
import spade.network._
import prism._

import scala.language.higherKinds
import scala.language.reflectiveCalls

package object node {

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
    case x:Pin[_] => x.bct
    case x:Bundle[_] => x.bct
    case x:StaticMeshNetwork[_] => x.bct
    case x:DynamicMeshNetwork[_] => x.bct
    case x:FIFO[_] => x.bct
    case x => throw PIRException(s"don't have ClassTag[_<:PinType] for $x")
  }

  def is[B<:PinType:ClassTag](x:Any) = implicitly[ClassTag[B]] == bctOf(x)

  def as[B<:PinType:ClassTag,A[_<:PinType]](x:A[_]) = if (is[B](x)) Some(x.asInstanceOf[A[B]]) else None

  def bundleOf[B<:PinType:ClassTag:TypeTag](x:SpadeNode) = {
    x.collectDown[Bundle[B]]().headOption
  }
  def cbundleOf(x:SpadeNode) = bundleOf[Bit](x)
  def sbundleOf(x:SpadeNode) = bundleOf[Word](x)
  def vbundleOf(x:SpadeNode) = bundleOf[Vector](x)

  def isMesh(n:Top) = n match {
    case n:MeshTop => true
    case _ => false
  }

  def isDynamic(n:Top) = n match {
    case n:DynamicMeshTop => true
    case n => false
  }

  def isStatic(n:Top) = n match {
    case n:StaticMeshTop => true
    case n => false
  }

  def cuOf(n:SpadeNode) = n.collectUp[CU]().headOption

  def routableOf(n:SpadeNode) = n.collectUp[Routable]().headOption

}
