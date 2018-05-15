package spade
package node

abstract class MeshTop(override val param:MeshTopParam)(implicit design:SpadeDesign) extends Top(param) {
  import param._
  import design.spademeta._

  @transient val bundleGroups = ListBuffer[BundleGroup]()

  def bundleGroup(param:Parameter, coord:Option[(Int,Int)]=None) = {
    val bg = BundleGroup(param,coord=coord)
    bundleGroups += bg
    bg
  }

}

case class StaticMeshTop(override val param:StaticMeshTopParam)(implicit design:SpadeDesign) extends MeshTop(param) {
  import param._
  import design.spademeta._

  val scale = 2

  @transient val argFringe = bundleGroup(fringePattern.argFringeParam, coord=None)

  @transient val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    bundleGroup(
      centrolPattern.cuAt(i,j), 
      coord=Some(i*scale+2, j*scale+1)
    )
  }

  @transient val sbArray = List.tabulate(numCols + 1, numRows + 1) { case (i,j) => 
    bundleGroup(
      centrolPattern.switchParam, 
      coord=Some(i*scale+1, j*scale)
    )
  }

  @transient val dramAGs = fringePattern.dagParam.map { dagParam =>
    List.tabulate(2, numRows+1) { case (i, j) => 
      bundleGroup(
        dagParam, 
        coord=Some(if (i==0) (0*scale, j*scale) else (numCols*scale+2, j*scale))
      )
    }
  }
  //@transient val sramAGs = List.tabulate(2, numRows+1) { case (i, j) => if (i==0) (-1, j) else (numCols, j) }
  @transient val mcArray = List.tabulate(2, numRows+1) { case (i, j) => 
    bundleGroup(
      fringePattern.mcParam, 
      coord=Some(if (i==0) (0*scale, j*scale) else (numCols*scale+2, j*scale))
    )
  }

  @transient val networks = networkParams.map { param => new StaticMeshNetwork(param, this) }

  bundleGroups.foreach { case BundleGroup(param, nios, coord) => 
    val m = Module(Factory.create(param, nios.toList))
    coord.foreach { coord => indexOf(m) = coord }
  }

}

case class DynamicMeshTop(override val param:DynamicMeshTopParam)(implicit design:SpadeDesign) extends MeshTop(param) {
  import param._
  import design.spademeta._

  @transient val argFringe = bundleGroup(fringePattern.argFringeParam)

  @transient val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    bundleGroup(centrolPattern.cuAt(i,j))
  }

  @transient val sbArray = List.tabulate(numCols, numRows) { case (i,j) => 
    bundleGroup(centrolPattern.switchParam)
  }

  //@transient val dramAGs = List.tabulate(2, numRows+1) { case (i, j) => BundleGroup(dramAGsParam, coord=Some(if (i==0) (-1, j) else (numCols, j))) }
  //@transient val sramAGs = List.tabulate(2, numRows+1) { case (i, j) => if (i==0) (-1, j) else (numCols, j) }
  @transient val mcArray = List.tabulate(2, numRows) { case (i, j) => 
    bundleGroup(fringePattern.mcParam)
  }

  @transient val networks = networkParams.map { param => new DynamicMeshNetwork(param, this) }

  bundleGroups.foreach { case BundleGroup(param, nios, coord) => 
    val m = Module(Factory.create(param, nios.toList))
    coord.foreach { coord => indexOf(m) = coord }
  }

}

case class BundleGroup(param:Parameter, nios:ListBuffer[GridBundle[_<:PinType]]=ListBuffer.empty, coord:Option[(Int,Int)]=None)
