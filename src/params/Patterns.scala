package spade
package node
                          
sealed trait Pattern extends Parameter

trait GridPattern extends Pattern

trait GridFringePattern extends GridPattern {
  val argFringeParam:ArgFringeParam
  val mcParam:MCParam
}

case class MCOnly(
  argFringeParam:ArgFringeParam=ArgFringeParam(),
  mcParam:MCParam=MCParam()
) extends GridFringePattern

case class MC_DramAG(
  argFringeParam:ArgFringeParam=ArgFringeParam(),
  mcParam:MCParam=MCParam(),
  dagParam:DramAGParam=DramAGParam()
) extends GridFringePattern

trait GridCentrolPattern extends GridPattern {
  val switchParam:SwitchParam
  def cuAt(i:Int, j:Int)(implicit top:MeshTop):CUParam
}

/*
 *
 *  +-----+-----+
 *  | PCU | PMU |
 *  +-----+-----+
 *  | PMU | PCU |
 *  +-----+-----+
 *
 * */
case class Checkerboard (
  switchParam:SwitchParam=SwitchParam(),
  pcuParam:PCUParam=PCUParam(),
  pmuParam:PMUParam=PMUParam()
) extends GridCentrolPattern {
  def cuAt(i:Int, j:Int)(implicit top:MeshTop) = {
    if ((i+j) % 2 == 0) pcuParam else pmuParam 
  }
}
/*
 *
 *  +-----+-----+
 *  | PCU | PMU |
 *  +-----+-----+
 *  | PCU | PMU |
 *  +-----+-----+
 *
 * */
case class ColumnStrip (
  switchParam:SwitchParam=SwitchParam(),
  pcuParam:PCUParam=PCUParam(),
  pmuParam:PMUParam=PMUParam()
) extends GridCentrolPattern {
  def cuAt(i:Int, j:Int)(implicit top:MeshTop) = {
    if (j % 2 == 0) pcuParam else pmuParam 
  }
}
/*
 *
 *  +-----+-----+
 *  | PCU | PCU |
 *  +-----+-----+
 *  | PMU | PMU |
 *  +-----+-----+
 *
 * */
case class RowStrip (
  switchParam:SwitchParam=SwitchParam(),
  pcuParam:PCUParam=PCUParam(),
  pmuParam:PMUParam=PMUParam()
) extends GridCentrolPattern {
  def cuAt(i:Int, j:Int)(implicit top:MeshTop) = {
    if (i % 2 == 0) pcuParam else pmuParam 
  }
}
/*
 *
 *  +-----+-----+
 *  | PCU | PMU |
 *  +-----+-----+
 *  | SCU | SCU |
 *  +-----+-----+
 *
 * */
case class MixAll (
  switchParam:SwitchParam=SwitchParam(),
  pcuParam:PCUParam=PCUParam(),
  pmuParam:PMUParam=PMUParam(),
  scuParam:SCUParam=SCUParam()
) extends GridCentrolPattern {
  def cuAt(i:Int, j:Int)(implicit top:MeshTop) = {
    if (i % 2 == 0) {
      if (j % 2 == 0) pcuParam else pmuParam
    } else scuParam
  }
}
/*
 *
 *  +-----+-----+
 *  | PCU | PMU |
 *  +-----+-----+
 *  | PMU | SCU |
 *  +-----+-----+
 *
 * */
case class HalfAndHalf (
  switchParam:SwitchParam=SwitchParam(),
  pcuParam:PCUParam=PCUParam(),
  pmuParam:PMUParam=PMUParam(),
  scuParam:SCUParam=SCUParam()
) extends GridCentrolPattern {
  def cuAt(i:Int, j:Int)(implicit top:MeshTop) = {
    if (i % 2 == 0) if (j % 2 == 0) pcuParam else pmuParam
    else if (j % 2 == 0) pmuParam else scuParam
  }
}
