package spade.newnode

import spade._
import prism.node._
import pirc.enums._

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable._

object Factory {
  def create(param:Any)(implicit design:Spade) = param match {
    case param:MeshTopParam => MeshTop(param)
  }
  def create(param:Any, nios:List[NetworkBundle[_]])(implicit design:Spade) = param match {
    case param:PCUParam => PCU(param, nios)
    case param:PMUParam => PMU(param, nios)
    case param:SCUParam => SCU(param, nios)
  }
}
