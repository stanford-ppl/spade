package spade.newnode

import spade._
import prism.node._
import pirc.enums._

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable._

trait DesignParam extends Parameter
abstract class Design(param:DesignParam) extends SpadeNode(0) with prism.node.Design with Module {
  implicit val design:this.type = this
}
