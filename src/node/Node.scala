package spade.node

import spade._
import spade.util._

import scala.collection.mutable.ListBuffer

/* Spade Node */
class Node(implicit val design:Spade) { 
  val spademeta = design.spademeta
  import spademeta._
  val id : Int = design.nextId
  override def equals(that: Any) = that match {
    case n: Node => super.equals(that) && id == n.id
    case _ => super.equals(that)
  }
  def typeStr = this.getClass().getSimpleName()
  override def toString = s"${typeStr}${id}" 
  def index(i:Int)(implicit design:Spade):this.type = { indexOf(this) = i; this }
  def index(implicit design:Spade):Int = { indexOf(this) }
  def coord(c:(Int, Int))(implicit design:Spade):this.type = { coordOf(this) = c; this} // Coordinate
  def coord(implicit design:Spade):(Int, Int) = { coordOf(this) }

  def isConst = this.isInstanceOf[Const[_]]
}
