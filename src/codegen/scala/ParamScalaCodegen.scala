package spade.codegen

import spade._
import spade.params._

import prism._
import prism.codegen._
import prism.collection.mutable._

class ParamScalaCodegen(val fileName:String)(implicit compiler:Spade) extends ParamCodegen with ScalaCodegen {

  val shouldRun = Config.codegen
  val forward = true

  override def quote(n:Any):String = n match {
    case n:Parameter => s"x${n.id}"
    case n:Table[_,_,_] => s"table${n.hashCode}"
    case n:String => s""""$n""""
    case n => super.quote(n)
  }

  override def emitNode(n:N) = {
    n.values.foreach{ case n:Table[_,_,_] => emitTable(n); case _ => }
    emitCaseClassInst(n)
  }

  def emitTable(n:Table[_,_,_]) = {
    emitInst(s"val ${quote(n)}_map = Map") { ms =>
      n.map.foreach { case (k, v) =>
        emitln(s"${quote(k)} -> ${quote(v)}")
      }
    }("")
    emitln(s"val ${quote(n)} = new Table(${quote(n)}_map) with ChannelWidth")
  }

  override def initPass(runner:RunPass[_]) = {
    super.initPass(runner)
    emitln(s"package spade.params")
    emitln
  }

  override def runPass(runner:RunPass[_]) = {
    emitBlock(s"trait GeneratedParam") {
      emitln
      emitln
      super.runPass(runner)
    }
  }
}
