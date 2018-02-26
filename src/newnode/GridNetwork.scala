package spade.newnode

import spade._
import prism.node._
import pirc.enums._
import pirc.collection.mutable.Table

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable._

case class GridBundle[B<:BundleType:ClassTag]()(implicit design:Spade) extends NetworkBundle[B] {
  private val inMap = Map[String, ListBuffer[Input[B]]]()
  private val outMap = Map[String, ListBuffer[Output[B]]]()

  def inputs = GridBundle.eightDirections.flatMap { dir => inMap.getOrElse(dir,Nil).toList } 
  def outputs = GridBundle.eightDirections.flatMap { dir => outMap.getOrElse(dir, Nil).toList }  

  def addInAt(dir:String, num:Int)(implicit design:Spade):List[Input[B]] = { 
    val ios = List.fill(num)(Input[B]("in"))
    inMap.getOrElseUpdate(dir, ListBuffer.empty) ++= ios
    ios
  }
  def addOutAt(dir:String, num:Int)(implicit design:Spade):List[Output[B]] = {
    val ios = List.fill(num)(Output[B]("out"))
    outMap.getOrElseUpdate(dir, ListBuffer.empty) ++= ios
    ios
  }
}
object GridBundle {
  def fourDirections = { "W" :: "N" :: "E" :: "S" ::Nil }
  def eightDirections = { "W" :: "NW" :: "N" :: "NE" :: "E" ::  "SE" :: "S" :: "SW" ::Nil }
  def diagDirections = {"NW":: "NE":: "SE":: "SW" :: Nil}
}

