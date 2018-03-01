package spade.pass

import spade._
import spade.node._

import prism._

import scala.collection.mutable

abstract class SpadePass(implicit override val compiler:Spade) extends prism.Pass {

  lazy val spademeta: SpadeMetadata = compiler.top.spademeta
  import spademeta._

  def quote(n:Any):String = n match {
    case n:SpadeNode => 
      s"${nameOf.get(n).getOrElse(n.className)}${n.id}${indexOf.get(n).fold("")(idx => s"[$idx]")}"
    case _ => n.toString
  }

}
