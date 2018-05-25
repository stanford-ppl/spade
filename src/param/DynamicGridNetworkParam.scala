package spade
package param

import prism.node._
import prism.collection.mutable.Table

abstract class DynamicGridNetworkParam[B<:PinType:ClassTag] extends NetworkParam[B] {
  lazy val topParam = collectOut[GridTopParam]().head
  lazy val numRows:Int = topParam.numRows
  lazy val numCols:Int = topParam.numCols
  lazy val argFringeParam = topParam.fringePattern.argFringeParam
  lazy val numArgIns:Int = argFringeParam.numArgIns
  lazy val numArgOuts:Int = argFringeParam.numArgOuts
  lazy val numTokenOuts:Int = argFringeParam.numTokenOuts
  val channelWidth:ChannelWidth
  val numVirtualClasses:Int
  val isTorus:Boolean
  val isMesh = !isTorus

  trait ChannelWidth extends Table[String, String, Int]
  object ChannelWidth {
    def empty = new Table[String, String, Int] (
      values=Map(
        "src"->cuTypes, 
        "dst"->cuTypes
      ), 
      default=Some(0)
    ) with ChannelWidth
  }

}

case class DynamicGridControlNetworkParam(
  numVirtualClasses:Int = 4,
  isTorus:Boolean=false
) extends DynamicGridNetworkParam[Bit] {
  override lazy val channelWidth = {
    val channelWidth = ChannelWidth.empty
    // switch to CU channel width
    channelWidth("src"->"rt", "dst"->List("pcu", "pmu", "scu")) = 4

    // CU to Switch channel width
    channelWidth("src"->List("pcu", "pmu", "scu"), "dst"->"rt") = 4

    // DAG to switch channel width
    channelWidth("src"->"dag", "dst"->"rt") = 1

    // switch to DAG channel width
    channelWidth("src"->"rt", "dst"->"dag") = 1

    // switch to MC channel width
    channelWidth("src"->"rt", "dst"->"mc") = 1

    // MC to switch channel width
    channelWidth("src"->"mc", "dst"->"rt") = 2

    // MC to DAG channel width
    channelWidth("src"->"mc", "dst"->"dag") = 2
      
    // Top to switch channel width
    channelWidth("src"->"arg", "dst"->"rt") = 1
    // switch to Top channel width
    channelWidth("src"->"rt", "dst"->"arg") = numTokenOuts
    channelWidth
  }
}

case class DynamicGridScalarNetworkParam(
  numVirtualClasses:Int = 4,
  isTorus:Boolean=false
) extends DynamicGridNetworkParam[Word] {
  override lazy val channelWidth = {
    val channelWidth = ChannelWidth.empty
    // switch to PCU channel width
    channelWidth("src"->"rt", "dst"->List("pcu", "scu")) = 4//roundUp(pcuSins / 4.0) 

    // PCU to Switch channel width
    channelWidth("src"->List("pcu", "scu"), "dst"->"rt") = 4//roundUp(pcuSouts / 4.0)

    // switch to PMU channel width
    channelWidth("src"->"rt", "dst"->List("pmu")) = 4//roundUp(pmuSins / 4.0) 

    // PMU to Switch channel width
    channelWidth("src"->List("pmu"), "dst"->"rt") = 4//roundUp(pmuSouts / 4.0)
    
    // switch to DAG channel width
    channelWidth("src"->"rt", "dst"->"dag") = 1//roundUp(ucuSins)

    // DAG to switch channel width
    channelWidth("src"->"dag", "dst"->"rt") = 1//roundUp(ucuSouts) - 2

    // switch to SAG channel width
    channelWidth("src"->"rt", "dst"->"pcu") = 4 

    // SAG to switch channel width
    channelWidth("src"->"pcu", "dst"->"rt") = 2 

    // switch to MC channel width
    channelWidth("src"->"rt", "dst"->"mc") = 3

    // MC to switch channel width
    channelWidth("src"->"mc", "dst"->"rt") = 1
      
    // DAG to MC channel width
    channelWidth("src"->"dag", "dst"->"mc") = 2
    
    //// switch to OCU channel width
    channelWidth("src"->"rt", "dst"->"ocu") = 5
    
    //// Top to switch channel width
    channelWidth("src"->"arg", "dst"->"rt") = numArgIns

    //// switch to Top channel width
    channelWidth("src"->"rt", "dst"->"arg") = numArgOuts
    channelWidth
  }
}

case class DynamicGridVectorNetworkParam(
  numVirtualClasses:Int = 4,
  isTorus:Boolean=false
) extends DynamicGridNetworkParam[Vector] {
  override lazy val channelWidth = {
    val channelWidth = ChannelWidth.empty
    // switch to PCU channel width
    channelWidth("src"->"rt", "dst"->List("pcu")) = 4//roundUp(pcuVins / 4.0)

    // PCU to Switch channel width
    channelWidth("src"->List("pcu"), "dst"->"rt") = 4//roundUp(pcuVouts / 4.0)

    // switch to PMU channel width
    channelWidth("src"->"rt", "dst"->List("pmu")) = 4//roundUp(pmuVins / 4.0) 

    // PMU to Switch channel width
    channelWidth("src"->List("pmu"), "dst"->"rt") = 4//roundUp(pmuVouts / 4.0)

    // switch to MC channel width
    channelWidth("src"->"rt", "dst"->"mc") = 1
      
    // MC to switch channel width
    channelWidth("src"->"mc", "dst"->"rt") = 1
    channelWidth
  }
}

