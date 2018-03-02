package spade.codegen

import spade.traversal._

import prism.traversal._
import prism.codegen._

import spade._

abstract class SpadeCodegen(implicit compiler:Spade) extends SpadeTraversal with ChildFirstTraversal with Codegen {
  override def runPass = {
    traverseNode(compiler.design.top)
  }
}
