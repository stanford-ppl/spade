package spade

import spade.node._

import prism._
import prism.enums._

import scala.language.existentials
import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}
import scala.language.higherKinds

package object util {

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
