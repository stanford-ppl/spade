package spade.codegen

import spade._
import spade.params._

import prism._
import prism.codegen._

class ParamIRPrinter(val fileName:String)(implicit compiler:Spade) extends ParamCodegen with IRPrinter {

  val forward = true
  lazy val metadata = None 

  def qdef(n:Any) = quote(n)

}
