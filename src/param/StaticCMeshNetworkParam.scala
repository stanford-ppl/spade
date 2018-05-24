package spade
package param

import prism.node._
import prism.collection.mutable.Table

abstract class StaticCMeshNetworkParam[B<:PinType:ClassTag] extends NetworkParam[B] {
  lazy val topParam = collectOut[CMeshTopParam]().head
  lazy val numRows:Int = topParam.numRows
  lazy val numCols:Int = topParam.numCols
  lazy val argFringeParam = topParam.pattern.argFringeParam
  lazy val numArgIns:Int = argFringeParam.numArgIns
  lazy val numArgOuts:Int = argFringeParam.numArgOuts
  lazy val numTokenOuts:Int = argFringeParam.numTokenOuts
  val channelWidth:ChannelWidth

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

case class StaticCMeshControlNetworkParam() extends StaticCMeshNetworkParam[Bit] {
  override lazy val channelWidth = {
    val channelWidth = ChannelWidth.empty
    // switch to switch channel width
    channelWidth("src"->"sb", "dst"->"sb") = 6

    // switch to CU channel width
    channelWidth("src"->"sb", "dst"->List("pcu", "mu", "pmu", "scu")) = 1

    // CU to Switch channel width
    channelWidth("src"->List("pcu", "mu", "pmu", "scu"), "dst"->"sb") = 2

    // DAG to switch channel width
    channelWidth("src"->"dag", "dst"->"sb") = 1

    // switch to DAG channel width
    channelWidth("src"->"sb", "dst"->"dag") = 1

    // switch to MC channel width
    channelWidth("src"->"sb", "dst"->"mc") = 1

    // MC to switch channel width
    channelWidth("src"->"mc", "dst"->"sb") = 2

    // MC to DAG channel width
    channelWidth("src"->"mc", "dst"->"dag") = 2
      
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

case class StaticCMeshScalarNetworkParam() extends StaticCMeshNetworkParam[Word] {
  override lazy val channelWidth = {
    val channelWidth = ChannelWidth.empty
    // switch to switch channel width
    channelWidth("src"->"sb", "dst"->"sb") = 4

    // switch to PCU channel width
    channelWidth("src"->"sb", "dst"->List("pcu", "scu")) = 1

    // PCU to Switch channel width
    channelWidth("src"->List("pcu", "scu"), "dst"->"sb") = 1

    // switch to MCU channel width
    channelWidth("src"->"sb", "dst"->List("pmu")) = 1

    // MCU to Switch channel width
    channelWidth("src"->List("pmu"), "dst"->"sb") = 1

    // switch to DAG channel width
    channelWidth("src"->"sb", "dst"->"dag") = 1

    // DAG to switch channel width
    channelWidth("src"->"dag", "dst"->"sb") = 1

    // switch to MC channel width
    channelWidth("src"->"sb", "dst"->"mc") = 3

    // MC to switch channel width
    channelWidth("src"->"mc", "dst"->"sb") = 1

    // DAG to MC channel width
    channelWidth("src"->"dag", "dst"->"mc") = 2

    //// switch to OCU channel width
    channelWidth("src"->"sb", "dst"->"ocu") = 5

    //// Top to switch channel width
    channelWidth("src"->"arg", "dst"->"sb") = numArgIns

    //// switch to Top channel width
    channelWidth("src"->"sb", "dst"->"arg") = numArgOuts
    channelWidth
  }
}

case class StaticCMeshVectorNetworkParam() extends StaticCMeshNetworkParam[Vector] {
  override lazy val channelWidth = {
    val channelWidth = ChannelWidth.empty
    // switch to switch channel width
    channelWidth("src"->"sb", "dst"->"sb") = 4

    // switch to PCU channel width
    channelWidth("src"->"sb", "dst"->List("pcu")) = 1

    // PCU to Switch channel width
    channelWidth("src"->List("pcu"), "dst"->"sb") = 1

    // switch to MCU channel width
    channelWidth("src"->"sb", "dst"->List("pmu")) = 1

    // MCU to Switch channel width
    channelWidth("src"->List("pmu"), "dst"->"sb") = 1

    // switch to MC channel width
    channelWidth("src"->"sb", "dst"->"mc") = 1

    // MC to switch channel width
    channelWidth("src"->"mc", "dst"->"sb") = 1
    channelWidth
  }
}
