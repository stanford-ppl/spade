package spade
package codegen

import prism.traversal._

abstract class SpadeCodegen(implicit compiler:Spade) extends SpadeTraversal with ChildFirstTraversal with Codegen {
  override def runPass = {
    traverseNode(compiler.top)
  }

  override def quote(n:Any):String = {
    super[SpadeTraversal].quote(n)
  }
}
