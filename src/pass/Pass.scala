package spade.pass

import spade._
import spade.node._

import pirc._

import scala.collection.mutable

abstract class SpadePass(implicit val compiler:Spade) extends pirc.Pass {

  lazy val spademeta: SpadeMetadata = compiler.top.spademeta

  def quote(n:Any):String = n match {
    case n:SpadeNode => spade.util.quote(n)
    case _ => n.toString
  }

}
