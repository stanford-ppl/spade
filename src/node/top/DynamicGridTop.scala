package spade
package node
import param._

case class DynamicGridTop(
  override val param:DynamicGridTopParam
)(implicit design:SpadeDesign) extends GridTop {
  import param._
  import design.spademeta._

  @transient val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    if (i == numCols/2-fringeNumCols && j == numRows-1) {
      bundleGroup(fringePattern.argFringeParam, coord=Some(i+fringeNumCols,j))
    } else {
      bundleGroup(
        centrolPattern.cuAt(i,j),
        coord=Some(i+fringeNumCols, j)
      )
    }
  }

  @transient val rtArray = List.tabulate(numTotalCols, numTotalRows) { case (i,j) => 
    bundleGroup(
      routerParam,
      coord=Some(i,j)
    )
  }

  val rtrx = rtArray.last.head.coord.get._1
  val rtlx = rtArray.head.head.coord.get._1
  val rtuy = rtArray.head.last.coord.get._2
  val rtby = rtArray.head.head.coord.get._2

  @transient val dagArray = fringePattern.dagParam.map { dagParam =>
    List.tabulate(2, numRows) { case (i, j) => 
      bundleGroup(
        dagParam, 
        coord=Some(if (i==0) (1, j) else (rtrx-1, j))
      ) 
    }
  }

  @transient val mcArray = List.tabulate(2, numRows) { case (i, j) => 
    bundleGroup(
      fringePattern.mcParam,
      coord=Some(if (i==0) (0,j) else (rtrx,j))
    )
  }

  @transient val networks = networkParams.map { param => Factory.create(param, this) }

  createSubmodules
}
