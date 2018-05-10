package spade
package codegen

class ParamIRPrinter(val fileName:String)(implicit compiler:Spade) extends ParamCodegen with IRPrinter {

  val forward = true
  lazy val metadata = None 

  def qdef(n:Any) = quote(n)

}
