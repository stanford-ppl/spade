package spade
package node

case class StaticCMeshTop(override val param:StaticCMeshTopParam)(implicit design:SpadeDesign) extends Top {
  import param._
  import design.spademeta._


  @transient val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    List.tabulate(2, 2) { case (ii, jj) =>
      bundleGroup(
        pattern.cuAt(i,j)(ii,jj), 
        coord=Some(i*3+ii*2, j*3+jj*2)
      )
    }
  }

  @transient val sbArray = List.tabulate(numCols, numRows) { case (i,j) => 
    bundleGroup(
      switchParam, 
      coord=Some(i*3+1, j*3+1)
    )
  }

  val sbrx = sbArray.last.head.coord.get._1
  val sblx = sbArray.head.head.coord.get._1
  val sbuy = sbArray.head.last.coord.get._2
  val sbby = sbArray.head.head.coord.get._2

  @transient val argFringe = bundleGroup(pattern.argFringeParam, coord=Some(((sbrx + sblx)/2), sbuy+2))

  @transient val networks = networkParams.map { param => new StaticCMeshNetwork(param, this) }

  createSubmodules
}

//case class DynamicCMeshTop(override val param:DynamicMeshTopParam)(implicit design:SpadeDesign) extends CMeshTop(param) {
  //import param._
  //import design.spademeta._

  //@transient val argFringe = bundleGroup(fringePattern.argFringeParam, coord=Some(numTotalCols / 2, numRows))

  //@transient val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    //bundleGroup(
      //centrolPattern.cuAt(i,j),
      //coord=Some(i+fringeNumCols, j)
    //)
  //}

  //@transient val rtArray = List.tabulate(numTotalCols, numTotalRows) { case (i,j) => 
    //bundleGroup(
      //routerParam,
      //coord=Some(i,j)
    //)
  //}

  //val rtrx = rtArray.last.head.coord.get._1
  //@transient val dagArray = fringePattern.dagParam.map { dagParam =>
    //List.tabulate(2, numRows) { case (i, j) => 
      //bundleGroup(
        //dagParam, 
        //coord=Some(if (i==0) (1, j) else (rtrx-1, j))
      //) 
    //}
  //}

  //@transient val mcArray = List.tabulate(2, numRows) { case (i, j) => 
    //bundleGroup(
      //fringePattern.mcParam,
      //coord=Some(if (i==0) (0,j) else (rtrx,j))
    //)
  //}

  ////@transient val networks = networkParams.map { param => new DynamicMeshNetwork(param, this) }

  //createSubmodules
//}
