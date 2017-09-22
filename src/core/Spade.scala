package spade

import spade.node._
import spade.util._
import spade.codegen._

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

  val configs = List(Config, SpadeConfig)

  lazy val simulatable = ListBuffer[Simulatable]()

  lazy val top = Top(topParam)

  var nextSym = 0
  def nextId = {val temp = nextSym; nextSym +=1; temp}
  
  override def reset = {
    super[SpadeMetadata].reset
    super[Design].reset
  }

  def handle(e:Exception):Unit = throw e

  def main(args: Array[String]): Unit = {
    info(s"args=[${args.mkString(", ")}]")
    reset
    setArgs(args)
    top.config
    endInfo(s"Finishing graph construction for ${this}")
    run
  }

  /* Codegen */
  val spadeNetworkCodegen = new SpadeNetworkCodegen()
  val spadeParamCodegen = new SpadeParamCodegen()

  /* Debug */
  val spadePrinter = new SpadePrinter()

  // Debug
  passes += spadePrinter 

  // Codegen
  passes += spadeNetworkCodegen 
  passes += spadeParamCodegen 

}
