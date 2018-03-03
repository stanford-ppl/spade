package spade.pass

import spade._
import spade.node._

import prism._
import prism.traversal._

trait SpadeCollector extends GraphCollector {

  def collectUp[M<:SpadeNode:ClassTag](n:SpadeNode, depth:Int= -1, visitFunc:SpadeNode => List[SpadeNode] = visitUp _, logger:Option[Logging]=None):List[M] =
    super.collect[SpadeNode, M](n, depth, visitFunc, logger)

  def collectDown[M<:SpadeNode:TypeTag:ClassTag](n:SpadeNode, depth:Int= -1, visitFunc:SpadeNode => List[SpadeNode] = visitDown _, logger:Option[Logging]=None):List[M] = {
    typeOf[M] match {
      case tag if tag <:< typeOf[Routable] => 
        // Performance optimization
        super.collect[SpadeNode, Routable](n, depth, visitFunc, logger).collect { case m:M => m }
      case _ => super.collect[SpadeNode, M](n, depth, visitFunc, logger)
    }
  }

  def collectIn[M<:SpadeNode:ClassTag](n:SpadeNode, depth:Int= -1, visitFunc:SpadeNode => List[SpadeNode] = visitLocalIn _, logger:Option[Logging]=None):List[M] = 
    super.collect[SpadeNode, M](n, depth, visitFunc, logger)

  def collectOut[M<:SpadeNode:ClassTag](n:SpadeNode, depth:Int= -1, visitFunc:SpadeNode => List[SpadeNode] = visitLocalOut _, logger:Option[Logging]=None):List[M] = 
    super.collect[SpadeNode, M](n, depth, visitFunc, logger)

  def cuOf(n:SpadeNode) = {
    collectUp[CU](n).headOption
  }

}

