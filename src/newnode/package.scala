package spade

import scala.reflect._
import scala.language.higherKinds

package object newnode {
  def isControl[B<:BundleType:ClassTag] = implicitly[ClassTag[B]] match {
    case tag if tag == classTag[Bit] => true
    case _ => false
  }
  def isScalar[B<:BundleType:ClassTag] = implicitly[ClassTag[B]] match {
    case tag if tag == classTag[Word] => true
    case _ => false
  }
  def isVector[B<:BundleType:ClassTag] = implicitly[ClassTag[B]] match {
    case tag if tag == classTag[Vector] => true
    case _ => false
  }

  def asControl[B<:BundleType:ClassTag,A[_<:BundleType]](x:A[B]) = if (isControl[B]) Some(x.asInstanceOf[A[Bit]]) else None
  def asScalar[B<:BundleType:ClassTag,A[_<:BundleType]](x:A[B]) = if (isScalar[B]) Some(x.asInstanceOf[A[Word]]) else None
  def asVector[B<:BundleType:ClassTag,A[_<:BundleType]](x:A[B]) = if (isVector[B]) Some(x.asInstanceOf[A[Vector]]) else None

  def block[T](lambda:Spade => T)(implicit spade:Spade):T = {
    lambda(spade)
  }

}
