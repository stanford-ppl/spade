package spade.codegen

import spade.pass._

import prism.traversal._
import prism.codegen._

import spade._

abstract class SpadeCodegen(implicit compiler:Spade) extends SpadeTraversal with ChildFirstTraversal with Codegen {
  override def runPass = {
    traverseNode(compiler.top)
  }

  override def quote(n:Any):String = {
    super[SpadeTraversal].quote(n)
  }
}
