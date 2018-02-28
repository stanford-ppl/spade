package spade.traversal

import spade._
import spade.node._
import spade.pass._

import prism.traversal._
import scala.collection.mutable.Set

trait SpadeTraversal extends SpadePass with SpadeWorld with prism.traversal.Traversal
