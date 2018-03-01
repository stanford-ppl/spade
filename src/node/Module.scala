package spade.node

import spade.params._
import prism.node._
import prism.enums._

abstract class Module(id:Int) extends SpadeNode(id) with SubGraph[SpadeNode] {
  def this()(implicit design:Design) = this(design.nextId)
  implicit val module:Module = this
}
object Module {
  def apply[M<:Module](module:M, name:String)(implicit parent:Module, design:Design):M = {
    module.setParent(parent)
  }
  def apply[M<:Module](module:M)(implicit parent:Module, design:Design):M = {
    module.setParent(parent)
  }
}

object Modules {
  def apply[M<:Module](name:String, modules:List[M])(implicit parent:Module, design:Design):List[M] = {
    modules.map(m => Module(m, name))
  }
  def apply[M<:Module](name:String,num:Int,lambda: => M)(implicit parent:Module, design:Design):List[M] = {
    List.tabulate(num)( i => Module(lambda, name) )
  }
}
