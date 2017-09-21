package spade

import spade.node._
import spade.util._

import pirc._
import pirc.util._

import scala.language.implicitConversions
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

trait SpadeParam {
  lazy val wordWidth = 32
  lazy val numLanes = 16
  lazy val clockFrequency:Int = 1000000000 //Hz
}

trait PreLoadSpadeParam extends SpadeParam {
  override lazy val numLanes = ConfigFactory.plasticineConf.lanes
}

trait Spade extends Design with SpadeMetadata with SpadeParam with SwitchNetwork {
  implicit def spade:this.type = this

  override def toString = getClass().getSimpleName().replace("$", "")

  lazy val top = Top(topParam)

  var nextSym = 0
  def nextId = {val temp = nextSym; nextSym +=1; temp}
  
  val simulatable = ListBuffer[Simulatable]()

  def config:Unit = top.config

  def handle(e:Exception):Unit = throw e
}
