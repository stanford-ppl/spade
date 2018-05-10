package spade
package node
                          
sealed trait Pattern extends Parameter

trait GridPattern extends Pattern {
  val step = 2 
}

trait GridCentrolPattern extends GridPattern {
  val switchParam:SwitchParam
  def switchAt(i:Int, j:Int)(implicit top:MeshTop):BundleGroup = {
    val coord = if (isDynamic(top)) (i,j) else (i*step, j*step)
    BundleGroup(param=switchParam, coord=Some(coord))
  }
  def cuAt(i:Int, j:Int)(implicit top:MeshTop):BundleGroup
  def cuCoord(i:Int, j:Int)(implicit top:MeshTop) = {
    top match {
      case top if isDynamic(top) => (i, j)
      case top => (step/2 + i*step, step/2 + j*step)
    }
  }
}

trait GridFringePattern extends GridPattern {
  val mcParam:MCParam
  val argFringeParam:ArgFringeParam
  def argBundle(implicit top:MeshTop):BundleGroup = {
    import top.param._
    top match {
      case top:StaticMeshTop => BundleGroup(argFringeParam)(top)
      case top:DynamicMeshTop => BundleGroup(argFringeParam, coord=Some(numCols/2, numRows))(top)
    }
  }
  def mcAt(i:Int, j:Int)(implicit top:MeshTop):BundleGroup = {
    import top.param._
    val coord = top match {
      case top:StaticMeshTop => if (i==0) (-step/2, j*step) else (numCols*step+step/2, j*step)
      case top:DynamicMeshTop => if (i==0) (-1, j) else (numCols, j)
    }
    BundleGroup(mcParam, coord=Some(coord))
  }
}

case class MCOnly(
  argFringeParam:ArgFringeParam=ArgFringeParam(),
  mcParam:MCParam=MCParam()
) extends GridFringePattern

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
  def cuAt(i:Int, j:Int)(implicit top:MeshTop):BundleGroup = {
    val param = if ((i+j) % 2 == 0) pcuParam else pmuParam 
    BundleGroup(param=param, coord=Some(cuCoord(i,j)))
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
  def cuAt(i:Int, j:Int)(implicit top:MeshTop):BundleGroup = {
    val param = if (j % 2 == 0) pcuParam else pmuParam 
    BundleGroup(param=param, coord=Some(cuCoord(i,j)))
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
  def cuAt(i:Int, j:Int)(implicit top:MeshTop):BundleGroup = {
    val param = if (i % 2 == 0) pcuParam else pmuParam 
    BundleGroup(param=param, coord=Some(cuCoord(i,j)))
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
  def cuAt(i:Int, j:Int)(implicit top:MeshTop):BundleGroup = {
    val param = if (i % 2 == 0) {
      if (j % 2 == 0) pcuParam else pmuParam
    } else scuParam
    BundleGroup(param=param, coord=Some(cuCoord(i,j)))
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
  def cuAt(i:Int, j:Int)(implicit top:MeshTop):BundleGroup = {
    val param = if (i % 2 == 0) if (j % 2 == 0) pcuParam else pmuParam
                else if (j % 2 == 0) pmuParam else scuParam
    BundleGroup(param=param, coord=Some(cuCoord(i,j)))
  }
}
