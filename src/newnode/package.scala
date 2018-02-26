package spade

import scala.reflect._

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
}
