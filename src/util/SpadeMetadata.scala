package spade.util

import spade._
import spade.node._

import pirc.util._
import pirc.collection.mutable._

trait SpadeMetadata extends Metadata { self:Spade =>

  /* Coordinate of a spade node in 2D array. Only defined for CUs and fringe. Used for pisa and dot codegen */
  object coordOf extends MOneToOneMap with MetadataMaps { 
    type K = Node
    type V = (Int, Int)
    override def reset = {} // Set during graph construction
  }
  
  /* Index of a spade node. Defined on list of fifos/counters/regs/stages  */
  object indexOf extends MOneToOneMap with MetadataMaps {
    type K = Node
    type V = Int
    override def reset = {} // Set during graph construction
  }
  
  /* Can a counter be the inner most counter in a chain */
  object isInnerCounter extends MOneToOneMap with MetadataMaps {
    type K = Node
    type V = Boolean 
    override def reset = {} // Set during graph construction
  }

  /* GridNetork of GlobalIO 
   * Whether the IO belongs to Vector/Scalar/Control network
   * */
  object networkOf extends MOneToOneMap with MetadataMaps {
    type K = GlobalIO[_<:PortType, _] 
    type V = GridNetwork
    override def reset = {} // Set during graph construction
  }

  /* Name of nodes */
  object nameOf extends MOneToOneMap with MetadataMaps {
    type K = Node
    type V = String 
    override def reset = {} // Set during graph construction
  }

}
