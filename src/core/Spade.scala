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
import java.io._

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

  var top:Top = _ 

  override def reset = {
    super[SpadeMetadata].reset
    super[Design].reset
    top = null
  }

  def handle(e:Exception):Unit = {
    logger.close
    throw e
  }

  def initDesign = {
    if (SpadeConfig.loadDesign) loadDesign else newDesign
  }

  def newDesign = {
    top = Top(topParam)
    top.connectAll
  }

  def loadDesign = {
    val path = s"${outDir}${File.separator}${name}.spade"
    val ois = new ObjectInputStream(new FileInputStream(path))
    top = ois.readObject.asInstanceOf[Top]
  }

  def saveDesign = {
    val path = s"${outDir}${File.separator}${name}.spade"
    val oos = new ObjectOutputStream(new FileOutputStream(path))
    oos.writeObject(top)
    oos.close
  }

  def main(args: Array[String]): Unit = {
    info(s"args=[${args.mkString(", ")}]")
    try {
      reset
      setArgs(args)
      initDesign
      endInfo(s"Finishing graph construction for ${this}")
      run
      if (SpadeConfig.saveDesign) saveDesign
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

  /* Debug */
  lazy val logger = new Logger() { override lazy val stream = newStream(s"spade.log") }
  lazy val spadePrinter = new SpadePrinter()
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

    super.run

    if (SpadeConfig.openDot) {
      plasticineVecDotPrinter.open
      plasticineScalDotPrinter.open
      plasticineCtrlDotPrinter.open
    }
  }

}
