package spade

import spade.node._
import spade.util._
import spade.codegen._
import spade.pass._

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
  val spademeta:SpadeMetadata = this

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

  def handle(e:Exception):Unit = {
    logger.close
    throw e
  }

  def main(args: Array[String]): Unit = {
    info(s"args=[${args.mkString(", ")}]")
    try {
      reset
      setArgs(args)
      top.connectAll
      endInfo(s"Finishing graph construction for ${this}")
      run
    } catch { 
      case e:Exception =>
        errmsg(e)
        handle(e)
    }
  }

  /* Analysis */
  //TODO: Area model

  /* Passes */
  lazy val areaModel = new AreaModel()

  /* Codegen */
  lazy val spadeNetworkCodegen = new SpadeNetworkCodegen()
  lazy val spadeParamCodegen = new SpadeParamCodegen()
  lazy val spadeControlCodegen = new SpadeControlCodegen()

  /* Debug */
  lazy val logger = new Logger() { override lazy val stream = newStream(s"spade.log") }
  // Printing of Plasticine IR connection in Text
  lazy val spadePrinter = new SpadePrinter()
  // Debugging dot graph of plasticine network topology.
  // After generation use bin/dot out/ArchName/DotFileName to open the graph
  lazy val plasticineVecDotPrinter = new PlasticineVectorDotPrinter()
  lazy val plasticineScalDotPrinter = new PlasticineScalarDotPrinter()
  lazy val plasticineCtrlDotPrinter = new PlasticineCtrlDotPrinter()

  override def run = {
    // Pass
    passes += areaModel 

    // Debug
    passes += spadePrinter 
    passes += plasticineVecDotPrinter 
    passes += plasticineScalDotPrinter 
    passes += plasticineCtrlDotPrinter 

    // Codegen
    passes += spadeNetworkCodegen 
    passes += spadeParamCodegen 
    passes += spadeParamCodegen 

    super.run

    if (SpadeConfig.openDot) {
      plasticineVecDotPrinter.open
      plasticineScalDotPrinter.open
      plasticineCtrlDotPrinter.open
    }
  }

}
