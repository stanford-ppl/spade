package spade.codegen

import spade.traversal._

import prism.traversal._
import prism.codegen._

import spade._

abstract class SpadeCodegen(implicit design:Spade) extends SpadeTraversal with ChildFirstTraversal with Codegen {
  val dirName = compiler.outDir

  override def runPass = {
    traverseNode(compiler.top)
  }
}
