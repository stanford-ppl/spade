package spade.pass

import spade._
import spade.node._

import prism._

import scala.collection.mutable

abstract class SpadePass(implicit override val compiler:Spade) extends prism.Pass with SpadeCollector {

  lazy val spademeta: SpadeMetadata = compiler.design.spademeta
  import spademeta._

  override def quote(n:Any):String = n match {
    case n:SpadeNode => 
      s"${nameOf.get(n).getOrElse(n.className)}${n.id}${indexOf.get(n).fold("")(indices => s"[${indices.mkString(",")}]")}"
    case _ => n.toString
  }

}
