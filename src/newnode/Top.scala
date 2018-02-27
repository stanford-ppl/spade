package spade.newnode

import spade._
import prism.node._
import pirc.enums._

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable._

trait TopParam extends Parameter
abstract class Top(param:TopParam)(implicit design:Spade) extends Module {
}

