package spade.node

import spade._
import spade.node._

import prism._
import prism.traversal._

trait SpadeCollector extends GraphCollector[SpadeNode] { self:SpadeNode =>

  override def collectDown[M<:SpadeNode:ClassTag:TypeTag](depth:Int= -1, logger:Option[Logging]=None):List[M] = {
    typeOf[M] match {
      case tag if tag <:< typeOf[Routable] => 
        // Performance optimization
        super.collectDown[Routable](depth, logger).collect { case m:M => m }
      case _ => super.collectDown[M](depth, logger)
    }
  }

}

