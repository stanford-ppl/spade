package spade
package node

import prism.node._
import prism.collection.mutable.Table

abstract class StaticMeshNetworkParam[B<:PinType:ClassTag] extends Parameter {
  val bct = implicitly[ClassTag[B]]
  lazy val meshTopParam = collectOut[MeshTopParam]().head
  lazy val numRows:Int = meshTopParam.numRows
  lazy val numCols:Int = meshTopParam.numCols
  lazy val argFringeParam = meshTopParam.fringePattern.argFringeParam
  lazy val numArgIns:Int = argFringeParam.numArgIns
  lazy val numArgOuts:Int = argFringeParam.numArgOuts
  lazy val numTokenOuts:Int = argFringeParam.numTokenOuts
  val channelWidth:Table[String,String,Int]
}

object ChannelWidth {
  def empty = new Table[String, String, Int] (
    values=Map(
      "src"->List("arg", "pcu", "ocu", "pmu", "mu", "scu", "mc", "sb"), 
      "dst"->List("arg", "pcu", "ocu", "pmu", "mu", "scu", "mc", "sb"), 
      "srcDir"->GridBundle.eightDirections, 
      "dstDir"->GridBundle.eightDirections
    ), 
    default=Some(0)
  )
}

case class StaticMeshControlNetworkParam() extends StaticMeshNetworkParam[Bit] {
  override lazy val channelWidth = {
    val channelWidth = ChannelWidth.empty
    // switch to switch channel width
    channelWidth("src"->"sb", "dst"->"sb") = 6

    // switch to CU channel width
    channelWidth("src"->"sb", "dst"->List("pcu", "mu", "pmu", "scu")) = 1

    // CU to Switch channel width
    channelWidth("src"->List("pcu", "mu", "pmu", "scu"), "dst"->"sb") = 2

    // DAG to switch channel width
    channelWidth("src"->"scu", "dst"->"sb") = 1

    // switch to DAG channel width
    channelWidth("src"->"sb", "dst"->"scu") = 1

    // switch to SAG channel width
    channelWidth("src"->"sb", "dst"->"pcu") = 2

    // SAG to switch channel width
    channelWidth("src"->"pcu", "dst"->"sb") = 2 

    // switch to MC channel width
    channelWidth("src"->"sb", "dst"->"mc") = 1

    // MC to switch channel width
    channelWidth("src"->"mc", "dst"->"sb") = 2

    // MC to DAG channel width
    channelWidth("src"->"mc", "dst"->"scu") = 2
      
    // OCU to switch channel width
    channelWidth("src"->"ocu", "dst"->"sb") = 2
    // switch to OCU channel width
    channelWidth("src"->"sb", "dst"->"ocu") = 4

    // Top to switch channel width
    channelWidth("src"->"arg", "dst"->"sb") = 1
    // switch to Top channel width
    channelWidth("src"->"sb", "dst"->"arg") = numTokenOuts
    channelWidth
  }
}

case class StaticMeshScalarNetworkParam() extends StaticMeshNetworkParam[Word] {
  override lazy val channelWidth = {
    val channelWidth = ChannelWidth.empty
    // switch to switch channel width
    channelWidth("src"->"sb", "dst"->"sb") = 4

    // switch to PCU channel width
    channelWidth("src"->"sb", "dst"->List("pcu", "scu")) = 1//roundUp(pcuSins / 4.0) 

    // PCU to Switch channel width
    channelWidth("src"->List("pcu", "scu"), "dst"->"sb") = 1//roundUp(pcuSouts / 4.0)

    // switch to MCU channel width
    channelWidth("src"->"sb", "dst"->List("pmu")) = 1//roundUp(pmuSins / 4.0) 

    // MCU to Switch channel width
    channelWidth("src"->List("pmu"), "dst"->"sb") = 1//roundUp(pmuSouts / 4.0)

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
    channelWidth("src"->"arg", "dst"->"sb") = numArgIns

    //// switch to Top channel width
    channelWidth("src"->"sb", "dst"->"arg") = numArgOuts
    channelWidth
  }
}

case class StaticMeshVectorNetworkParam() extends StaticMeshNetworkParam[Vector] {
  override lazy val channelWidth = {
    val channelWidth = ChannelWidth.empty
    // switch to switch channel width
    channelWidth("src"->"sb", "dst"->"sb") = 4

    // switch to PCU channel width
    channelWidth("src"->"sb", "dst"->List("pcu")) = 1//roundUp(pcuVins / 4.0)

    // PCU to Switch channel width
    channelWidth("src"->List("pcu"), "dst"->"sb") = 1//roundUp(pcuVouts / 4.0)

    // switch to MCU channel width
    channelWidth("src"->"sb", "dst"->List("pmu")) = 1//roundUp(pmuVins / 4.0) 

    // MCU to Switch channel width
    channelWidth("src"->List("pmu"), "dst"->"sb") = 1//roundUp(pmuVouts / 4.0)

    // switch to SAG channel width
    channelWidth("src"->"sb", "dst"->"pcu") = 4 

    // SAG to switch channel width
    channelWidth("src"->"pcu", "dst"->"sb") = 2 

    // switch to MC channel width
    channelWidth("src"->"sb", "dst"->"mc") = 1

    // MC to switch channel width
    channelWidth("src"->"mc", "dst"->"sb") = 1
    channelWidth
  }
}
