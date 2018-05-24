package spade
package node
import param._

case class DynamicMeshTop(
  override val param:DynamicMeshTopParam
)(implicit design:SpadeDesign) extends MeshTop {
  import param._
  import design.spademeta._

  @transient val argFringe = bundleGroup(fringePattern.argFringeParam, coord=Some(numTotalCols / 2, numRows))

  @transient val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    bundleGroup(
      centrolPattern.cuAt(i,j),
      coord=Some(i+fringeNumCols, j)
    )
  }

  @transient val rtArray = List.tabulate(numTotalCols, numTotalRows) { case (i,j) => 
    bundleGroup(
      routerParam,
      coord=Some(i,j)
    )
  }

  val rtrx = rtArray.last.head.coord.get._1
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

  @transient val networks = networkParams.map { param => new DynamicMeshNetwork(param, this) }

  createSubmodules
}
