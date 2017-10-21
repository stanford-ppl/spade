package spade.traversal

import spade._
import spade.node._

import scala.collection.mutable.Set

trait Traversal extends HiearchicalTraversal{

  val visited = Set[Any]()
  def reset {
    visited.clear
  }

  def traverse(implicit spade: Spade):Unit = {
    traverseDown(spade.top)
  }

  override def traverseDown(node:Any):Unit = {
    assert(!visited.contains(node), s"Revisiting visited node ${node}! visitedNodes:${visited}")
    if (visited.contains(node)) return
    super.traverseDown(node)
  }

  //def traverse(implicit spade: Spade):Unit = {
    //visitNode(spade.top)
    //spade.pcus.foreach(visitNode)
    //spade.mcus.foreach(visitNode)
    //spade.scus.foreach(visitNode)
    //spade.ocus.foreach(visitNode)
    //spade.mcs.foreach(visitNode)
    //spade match {
      //case spade:SwitchNetwork => spade.sbs.foreach(visitNode)
      //case _ =>
    //}
  //} 

  //def visitNode(node: Node) : Unit = {
    ////assert(!visited.contains(node), s"Revisiting visited node ${node}! visitedNodes:${visited}")
    //if (visited.contains(node)) return
    //visited += node
    //node match {
      //case n:Routable =>
        //n.gridIOs.foreach{_.ios.foreach(visitNode)}
        //n match {
          //case n:Controller => 
            //visitNode(n.ctrlBox)
            //n match {
              //case n:ComputeUnit =>
                //n.srams.foreach(visitNode)
                //n.ctrs.foreach(visitNode)
                //n.sfifos.foreach(visitNode)
                //n.vfifos.foreach(visitNode)
                //n.stages.foreach(visitNode)
              //case n:MemoryController =>
                //n.sfifos.foreach(visitNode)
                //n.vfifos.foreach(visitNode)
              //case n:Top =>
            //}
          //case n:SwitchBox =>
        //}
      //case n:GlobalIO[_,_] => visitNode(n.ic)
      //case n:SRAM => 
      //case n:Stage =>
        //visitNode(n.funcUnit)
        //n.prs.foreach(visitNode)
      //case n:CtrlBox =>
        //n.udcs.foreach(visitNode)
        //n.andTrees.foreach(visitNode)
        //n.andGates.foreach(visitNode)
        //n.delays.foreach(visitNode)
        //n.predicateUnits.foreach(visitNode)
        //n match {
          //case n:OuterCtrlBox => 
            //visitNode(n.udsm)
          //case n =>
        //}
      //case n:UpDownSM =>
        //visitNode(n.udc)
      //case n =>
    //}
    //node match {
      //case n:Module => n.ins.map(_.src).collect{case c:Const[_] => c}.foreach(visitNode)
      //case _ =>
    //}
  //}

}
