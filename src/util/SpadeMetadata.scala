package spade.util

import spade._
import spade.node._

import prism.util._
import prism.collection.mutable._

trait SpadeMetadata extends Metadata {

  /* Coordinate of a spade node. Used for pisa and dot codegen */
  object coordOf extends OneToOneMap[Node, (Int,Int)] with MetadataMap
  
  /* Index of a spade node. Used for pisa codegen */
  object indexOf extends OneToOneMap[Node, Int] with MetadataMap
  
  /* Can a counter be the inner most counter in a chain */
  object isInnerCounter extends OneToOneMap[Node, Boolean] with MetadataMap

  /* GridNetork of GlobalIO */
  object networkOf extends OneToOneMap[GlobalIO[_<:PortType, _], GridNetwork] with MetadataMap

  /* Name of nodes */
 //TODO
  object nameOf extends OneToOneMap[Any, String] with MetadataMap

}
