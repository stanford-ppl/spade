package spade.node

import spade._
import spade.node._

import prism._
import prism.traversal._

trait SpadeCollector extends GraphCollector[SpadeNode] { self:SpadeNode =>

  override def collectDown[M<:SpadeNode:ClassTag:TypeTag](depth:Int= -1, logger:Option[Logging]=None):List[M] = {
    // Performance optimization
    typeOf[M] match {
      case tag if tag <:< typeOf[Routable] => // Single layer of hiearchy
        super.collectDown[Routable](depth, logger).collect { case m:M => m }
      case tag if tag <:< typeOf[Bundle[_]] =>  // Shouldn't get submodule's bundle
        def visitDown(n:N):List[N] = n.children.collect { case c:M => c }
        super.collect[M](visitDown _, depth, logger)
      case tag if tag <:< typeOf[Module] => // No need to go inside the bundles
        def visitDown(n:N):List[N] = n.children.filterNot { _.isInstanceOf[Bundle[_]] }
        super.collect[M](visitDown _, depth, logger)
      case _ => super.collectDown[M](depth, logger)
    }
  }

}

