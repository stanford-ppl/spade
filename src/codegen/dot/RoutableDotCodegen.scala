package spade.codegen

import spade._
import spade.node._

import prism._
import prism.codegen._

class SpadeDotCodegen[M<:SpadeNode:ClassTag](val fileName:String)(implicit compiler:Spade) extends SpadeCodegen with IRDotCodegen {

  import spademeta._

  def getLabel(n:Any) = quote(n)

  //def shape(attr:DotAttr, n:Any) = attr.shape(box)

  //override def color(attr:DotAttr, n:Any) = n match {
    //case n => super.color(attr, n)
  //}

  override def emitNode(n:N) = {
    n match {
      case n => super.emitNode(n) 
    }
  }

  val node = collectDown[M](compiler.top).head

  override def runPass = {
    traverseNode(node)
  }

}
