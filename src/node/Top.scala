package spade
package node
import param._

trait Top extends Module {
  val param:TopParam
  import param._
  import design.spademeta._

  @transient val bundleGroups = ListBuffer[BundleGroup]()

  def bundleGroup(param:Parameter, coord:Option[(Int,Int)]=None) = {
    val bg = BundleGroup(param,coord=coord)
    bundleGroups += bg
    bg
  }

  def createSubmodules = {
    bundleGroups.foreach { case b@BundleGroup(param, coord) => 
      val m = Module(Factory.create(param, b.bundles))
      coord.foreach { coord => indexOf(m) = coord }
    }
  }

}
