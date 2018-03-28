package spade.node

import spade._

import prism._
import prism.node._
import prism.util._

import scala.language.reflectiveCalls

import scala.collection.mutable._

abstract class Bundle[B<:PinType:ClassTag]()(implicit design:Design) extends Module {
  val bct = implicitly[ClassTag[B]]
  def inputs:List[Input[B]]
  def outputs:List[Output[B]]
}
case class GridBundle[B<:PinType:ClassTag]()(implicit design:Design) extends Bundle[B] {
  import GridBundle._

  val inMap = Map[String, ListBuffer[Input[B]]]()
  val outMap = Map[String, ListBuffer[Output[B]]]()

  def inAt(dir:String) = inMap.get(dir).map { _.toList }.getOrElse(Nil)
  def outAt(dir:String) = outMap.get(dir).map { _.toList }.getOrElse(Nil)

  def inputs = eightDirections.flatMap { dir => inAt(dir) } 
  def outputs = eightDirections.flatMap { dir => outAt(dir) }  

  def addInAt(dir:String, num:Int)(implicit design:Design):List[Input[B]] = { 
    val ios = List.fill(num)(Input[B]("in"))
    inMap.getOrElseUpdate(dir, ListBuffer.empty) ++= ios
    ios
  }
  def addOutAt(dir:String, num:Int)(implicit design:Design):List[Output[B]] = {
    val ios = List.fill(num)(Output[B]("out"))
    outMap.getOrElseUpdate(dir, ListBuffer.empty) ++= ios
    ios
  }

  def addIns(num:Int)(implicit design:Design):List[Input[B]] = { 
    addInAt("W", num)
  }
  def addOuts(num:Int)(implicit design:Design):List[Output[B]] = {
    addOutAt("W", num)
  }
}
object GridBundle {
  val fourDirections = List("W","N","E","S")
  val diagDirections = List("NW","NE","SE","SW")
  val eightDirections = List("W", "NW", "N", "NE", "E", "SE", "S", "SW")
}
