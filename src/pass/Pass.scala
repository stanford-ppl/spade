package spade.pass

import spade._
import spade.node._

import scala.collection.mutable

abstract class Pass(implicit val design:Spade) extends pirc.pass.Pass {

  lazy val spademeta: SpadeMetadata = design

  def quote(n:Any):String = n match {
    case n:Node => spade.util.quote(n)
    case _ => n.toString
  }

}
