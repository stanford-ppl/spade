package spade.node

import scala.collection.mutable

abstract class Bundle[B<:PinType:ClassTag]()(implicit design:SpadeDesign) extends Module {
  val bct = implicitly[ClassTag[B]]
  def inputs:List[Input[B]]
  def outputs:List[Output[B]]
}
case class GridBundle[B<:PinType:ClassTag]()(implicit design:SpadeDesign) extends Bundle[B] {
  import GridBundle._

  val inMap = mutable.Map[String, ListBuffer[Input[B]]]()
  val outMap = mutable.Map[String, ListBuffer[Output[B]]]()

  def inAt(dir:String) = inMap.get(dir).map { _.toList }.getOrElse(Nil)
  def outAt(dir:String) = outMap.get(dir).map { _.toList }.getOrElse(Nil)

  def inputs = eightDirections.flatMap { dir => inAt(dir) } 
  def outputs = eightDirections.flatMap { dir => outAt(dir) }  

  def addInAt(dir:String, num:Int)(implicit design:SpadeDesign):List[Input[B]] = { 
    val ios = List.fill(num)(Input[B]("in"))
    inMap.getOrElseUpdate(dir, ListBuffer.empty) ++= ios
    ios
  }
  def addOutAt(dir:String, num:Int)(implicit design:SpadeDesign):List[Output[B]] = {
    val ios = List.fill(num)(Output[B]("out"))
    outMap.getOrElseUpdate(dir, ListBuffer.empty) ++= ios
    ios
  }

  def addIns(num:Int)(implicit design:SpadeDesign):List[Input[B]] = { 
    addInAt("W", num)
  }
  def addOuts(num:Int)(implicit design:SpadeDesign):List[Output[B]] = {
    addOutAt("W", num)
  }
}
object GridBundle {
  val fourDirections = List("W","N","E","S")
  val diagDirections = List("NW","NE","SE","SW")
  val eightDirections = List("W", "NW", "N", "NE", "E", "SE", "S", "SW")
}
