package spade
package pass

import spade._

import prism.traversal._

trait SpadeTraversal extends SpadePass with SpadeWorld with prism.traversal.Traversal { self:HierarchicalTraversal =>
  def top = compiler.top
}
