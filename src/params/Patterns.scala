package spade
package node
                          
sealed trait Pattern extends Parameter

trait GridPattern extends Pattern

trait GridFringePattern extends Pattern {
  val argFringeParam:ArgFringeParam
  val mcParam:MCParam
  val dagParam:Option[DramAGParam]
}

case class MCOnly(
  argFringeParam:ArgFringeParam=ArgFringeParam(),
  mcParam:MCParam=MCParam(),
  dagParam:Option[DramAGParam]=None
) extends GridFringePattern

case class MC_DramAG(
  argFringeParam:ArgFringeParam=ArgFringeParam(),
  mcParam:MCParam=MCParam(),
  dagParam:Option[DramAGParam]=Some(DramAGParam())
) extends GridFringePattern

trait GridCentrolPattern extends Pattern {
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
  pcuParam:PCUParam=PCUParam(),
  pmuParam:PMUParam=PMUParam(),
  scuParam:SCUParam=SCUParam()
) extends GridCentrolPattern {
  def cuAt(i:Int, j:Int)(implicit top:MeshTop) = {
    if (i % 2 == 0) if (j % 2 == 0) pcuParam else pmuParam
    else if (j % 2 == 0) pmuParam else scuParam
  }
}
