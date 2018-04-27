package spade.node

import prism._
import prism.node._
import prism.collection.mutable.Table

abstract class DynamicMeshNetworkParam[B<:PinType:ClassTag] extends Parameter {
  val bct = implicitly[ClassTag[B]]
  lazy val meshTopParam = collectOut[MeshTopParam]().head
  lazy val numRows:Int = meshTopParam.numRows
  lazy val numCols:Int = meshTopParam.numCols
  lazy val argFringeParam = meshTopParam.fringePattern.argFringeParam
  lazy val numArgIns:Int = argFringeParam.numArgIns
  lazy val numArgOuts:Int = argFringeParam.numArgOuts
  val channelWidth:DynamicChannelWidth
  val numVirtualClasses:Int
}

trait DynamicChannelWidth extends Table[String, String, Int]
object DynamicChannelWidth {
  def empty = new Table[String, String, Int] (
    values=Map(
      "src"->List("arg", "pcu", "ocu", "pmu", "scu", "mc", "sb"), 
      "dst"->List("arg", "pcu", "ocu", "pmu", "scu", "mc", "sb")
    ), 
    default=Some(0)
  ) with DynamicChannelWidth
}

case class DynamicMeshControlNetworkParam(
  channelWidth:DynamicChannelWidth=DynamicChannelWidth.empty,
  numVirtualClasses:Int = 4
) extends DynamicMeshNetworkParam[Bit] {
  // switch to CU channel width
  channelWidth("src"->"sb", "dst"->List("pcu", "pmu", "scu")) = 4

  // CU to Switch channel width
  channelWidth("src"->List("pcu", "pmu", "scu"), "dst"->"sb") = 4

  // DAG to switch channel width
  channelWidth("src"->"scu", "dst"->"sb") = 1

  // switch to DAG channel width
  channelWidth("src"->"sb", "dst"->"scu") = 1

  // switch to MC channel width
  channelWidth("src"->"sb", "dst"->"mc") = 1

  // MC to switch channel width
  channelWidth("src"->"mc", "dst"->"sb") = 2

  // MC to DAG channel width
  channelWidth("src"->"mc", "dst"->"scu") = 2
    
  // Top to switch channel width
  channelWidth("src"->"arg", "dst"->"sb") = 1
  // switch to Top channel width
  channelWidth("src"->"sb", "dst"->"arg") = 1
}

case class DynamicMeshScalarNetworkParam(
  channelWidth:DynamicChannelWidth=DynamicChannelWidth.empty,
  numVirtualClasses:Int = 4
) extends DynamicMeshNetworkParam[Word] {
  // switch to PCU channel width
  channelWidth("src"->"sb", "dst"->List("pcu", "scu")) = 4//roundUp(pcuSins / 4.0) 

  // PCU to Switch channel width
  channelWidth("src"->List("pcu", "scu"), "dst"->"sb") = 4//roundUp(pcuSouts / 4.0)

  // switch to PMU channel width
  channelWidth("src"->"sb", "dst"->List("pmu")) = 4//roundUp(pmuSins / 4.0) 

  // PMU to Switch channel width
  channelWidth("src"->List("pmu"), "dst"->"sb") = 4//roundUp(pmuSouts / 4.0)
  
  // switch to DAG channel width
  channelWidth("src"->"sb", "dst"->"scu") = 1//roundUp(ucuSins)

  // DAG to switch channel width
  channelWidth("src"->"scu", "dst"->"sb") = 1//roundUp(ucuSouts) - 2

  // switch to SAG channel width
  channelWidth("src"->"sb", "dst"->"pcu") = 4 

  // SAG to switch channel width
  channelWidth("src"->"pcu", "dst"->"sb") = 2 

  // switch to MC channel width
  channelWidth("src"->"sb", "dst"->"mc") = 3

  // MC to switch channel width
  channelWidth("src"->"mc", "dst"->"sb") = 1
    
  // DAG to MC channel width
  channelWidth("src"->"scu", "dst"->"mc") = 2
  
  //// switch to OCU channel width
  channelWidth("src"->"sb", "dst"->"ocu") = 5
  
  //// Top to switch channel width
  channelWidth("src"->"arg", "dst"->"sb") = 1

  //// switch to Top channel width
  channelWidth("src"->"sb", "dst"->"arg") = 1
}

case class DynamicMeshVectorNetworkParam(
  channelWidth:DynamicChannelWidth=DynamicChannelWidth.empty,
  numVirtualClasses:Int = 4
) extends DynamicMeshNetworkParam[Vector] {
  // switch to PCU channel width
  channelWidth("src"->"sb", "dst"->List("pcu")) = 4//roundUp(pcuVins / 4.0)

  // PCU to Switch channel width
  channelWidth("src"->List("pcu"), "dst"->"sb") = 4//roundUp(pcuVouts / 4.0)

  // switch to PMU channel width
  channelWidth("src"->"sb", "dst"->List("pmu")) = 4//roundUp(pmuVins / 4.0) 

  // PMU to Switch channel width
  channelWidth("src"->List("pmu"), "dst"->"sb") = 4//roundUp(pmuVouts / 4.0)

  // switch to MC channel width
  channelWidth("src"->"sb", "dst"->"mc") = 1
    
  // MC to switch channel width
  channelWidth("src"->"mc", "dst"->"sb") = 1
}

