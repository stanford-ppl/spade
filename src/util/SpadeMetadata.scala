package spade.util

import spade._
import spade.node._

import prism.util._
import prism.collection.mutable._

class SpadeMetadata extends Metadata {

  /* N-Dimential coordinate of a nodes.  */
  object indexOf extends OneToOneMap[SpadeNode, List[Int]] with MetadataMap {
    def update(k:K, v:Int):Unit = update(k, List(v))
    def update(k:K, v:(Int,Int)):Unit = update(k, List(v._1, v._2))
  }
  
  /* Can a counter be the inner most counter in a chain */
  object isInnerCounter extends OneToOneMap[SpadeNode, Boolean] with MetadataMap

  /* Name of nodes */
 //TODO
  object nameOf extends OneToOneMap[Any, String] with MetadataMap

}
