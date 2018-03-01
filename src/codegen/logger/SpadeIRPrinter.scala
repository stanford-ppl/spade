package spade.codegen
import spade._

import prism._
import prism.codegen._

class SpadeIRPrinter(val fileName:String)(implicit compiler:Spade) extends SpadeCodegen with IRPrinter {

  lazy val metadata = Some(spademeta)

  def qdef(n:Any) = quote(n)
}
