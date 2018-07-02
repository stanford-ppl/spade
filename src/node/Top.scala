package spade
package node
import param._

trait Top extends Module {
  val param:TopParam
  import param._
  import design.spademeta._

  @transient val bundleGroups = ListBuffer[BundleGroup]()

  @transient val networks:List[Network[_<:PinType]]

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

  private lazy val paramMap:Map[ClassTag[_], Map[Parameter, List[Bundle[_]]]] = networks.map { net => 
    val map = bundleGroups.groupBy { bg => bg.param }.map { case (param, bgs) =>
      (param, bgs.map { bg => bg.bundle(net.bct) }.toList)
    }.toMap
    (net.bct, map)
  }.toMap

  lazy val minInputSize = paramMap.mapValues { _.mapValues{ _.map{_.inputs.size}.min } }
  def minInputs[B<:PinType:ClassTag](param:Parameter) = 
    minInputSize.get(implicitly[ClassTag[B]]).map { _.getOrElse(param, 0) }.getOrElse(0)

  lazy val maxInputSize = paramMap.mapValues { _.mapValues{ _.map{_.inputs.size}.max } }
  def maxInputs[B<:PinType:ClassTag](param:Parameter) = 
    maxInputSize.get(implicitly[ClassTag[B]]).map { _.getOrElse(param, 0) }.getOrElse(0)

  lazy val minOutputSize = paramMap.mapValues { _.mapValues{ _.map{_.outputs.size}.min } }
  def minOutputs[B<:PinType:ClassTag](param:Parameter) = 
    minOutputSize.get(implicitly[ClassTag[B]]).map { _.getOrElse(param, 0) }.getOrElse(0)

  lazy val maxOutputSize = paramMap.mapValues { _.mapValues{ _.map{_.outputs.size}.max } }
  def maxOutputs[B<:PinType:ClassTag](param:Parameter) = 
    maxOutputSize.get(implicitly[ClassTag[B]]).map { _.getOrElse(param, 0) }.getOrElse(0)
}
