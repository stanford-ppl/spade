package spade.newnode

import spade._
import prism.node._
import pirc.enums._
import pirc.collection.mutable.Table

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable._

trait TopParam
abstract class Top(param:TopParam)(implicit spade:Spade) extends Module {
}

