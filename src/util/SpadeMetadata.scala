package spade.util

import spade._
import spade.node._

import prism.util._
import prism.collection.mutable._

class SpadeMetadata extends Metadata {

  /* Coordinate of a spade node. Used for pisa and dot codegen */
  object coordOf extends OneToOneMap[SpadeNode, (Int,Int)] with MetadataMap
  
  /* Index of a spade node. Used for pisa codegen */
  object indexOf extends OneToOneMap[SpadeNode, Int] with MetadataMap
  
  /* Can a counter be the inner most counter in a chain */
  object isInnerCounter extends OneToOneMap[SpadeNode, Boolean] with MetadataMap

  /* Name of nodes */
 //TODO
  object nameOf extends OneToOneMap[Any, String] with MetadataMap

}
