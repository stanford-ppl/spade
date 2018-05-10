package spade
package node

abstract class MeshTop(val param:MeshTopParam)(implicit design:SpadeDesign) extends Top(param) {
  import param._
  import design.spademeta._

  @transient val bundles = ListBuffer[BundleGroup]()

  @transient val argFringe = fringePattern.argBundle

  @transient val cuArray = List.tabulate(numCols, numRows) { case (i,j) => 
    centrolPattern.cuAt(i,j)
  }

}

case class StaticMeshTop(override val param:StaticMeshTopParam)(implicit design:SpadeDesign) extends MeshTop(param) {
  import param._
  import design.spademeta._

  @transient val sbArray = List.tabulate(numCols + 1, numRows + 1) { case (i,j) => 
    centrolPattern.switchAt(i,j)
  }

  //@transient val dramAGs = List.tabulate(2, numRows+1) { case (i, j) => BundleGroup(dramAGsParam, coord=Some(if (i==0) (-1, j) else (numCols, j))) }
  //@transient val sramAGs = List.tabulate(2, numRows+1) { case (i, j) => if (i==0) (-1, j) else (numCols, j) }
  @transient val mcArray = List.tabulate(2, numRows+1) { case (i, j) => 
    fringePattern.mcAt(i,j)
  }

  @transient val networks = networkParams.map { param => new StaticMeshNetwork(param, this) }

  bundles.foreach { case BundleGroup(param, nios, coord) => 
    val m = Module(Factory.create(param, nios.toList))
    coord.foreach { coord => indexOf(m) = coord }
  }

}

case class DynamicMeshTop(override val param:DynamicMeshTopParam)(implicit design:SpadeDesign) extends MeshTop(param) {
  import param._
  import design.spademeta._

  @transient val sbArray = List.tabulate(numCols, numRows) { case (i,j) => 
    centrolPattern.switchAt(i,j)
  }

  //@transient val dramAGs = List.tabulate(2, numRows+1) { case (i, j) => BundleGroup(dramAGsParam, coord=Some(if (i==0) (-1, j) else (numCols, j))) }
  //@transient val sramAGs = List.tabulate(2, numRows+1) { case (i, j) => if (i==0) (-1, j) else (numCols, j) }
  @transient val mcArray = List.tabulate(2, numRows) { case (i, j) => 
    fringePattern.mcAt(i,j)
  }

  @transient val networks = networkParams.map { param => new DynamicMeshNetwork(param, this) }

  bundles.foreach { case BundleGroup(param, nios, coord) => 
    val m = Module(Factory.create(param, nios.toList))
    coord.foreach { coord => indexOf(m) = coord }
  }

}
