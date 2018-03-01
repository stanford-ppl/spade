package spade.codegen

import spade._
import spade.params._

import prism._
import prism.codegen._
import prism.traversal._

abstract class ParamCodegen(implicit compiler:Spade) extends Pass with BFSTopologicalTraversal with Codegen with GraphCollector {
  type N = Parameter

  override def runPass = {
    val allNodes = new BFSTopologicalTraversal with GraphSchedular {
      type N = Parameter
      val forward = false
      override def traverseNode(n:N, prev:T) = traverse(List(n), prev)
    }.schedule(compiler.topParam).toSet.toList
    dbg(s"allNodes=$allNodes")
    traverse(scheduleDepFree(allNodes), ())
  }
  
}
