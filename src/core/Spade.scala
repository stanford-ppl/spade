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
  val spademeta:SpadeMetadata = this

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

  def load = SpadeConfig.loadDesign
  def save = SpadeConfig.saveDesign

  val designPath = s"${outDir}${File.separator}${name}.spade"

  def loadDesign = top = loadFromFile[Top](designPath)

  def newDesign = {
    top = Top(topParam)
    top.connectAll
  }

  def saveDesign:Unit = saveToFile(top, designPath)

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

  override def initSession = {
    super.initSession
    import session._
    // Pass
    addPass(areaModel)

    // Debug
    addPass(spadePrinter)
    addPass(plasticineVecDotPrinter)
    addPass(plasticineScalDotPrinter)
    addPass(plasticineCtrlDotPrinter)

    // Codegen
    addPass(spadeNetworkCodegen)
    addPass(spadeParamCodegen)
  }

  override def runSession = {
    super.runSession
    if (SpadeConfig.openDot) {
      plasticineVecDotPrinter.open
      plasticineScalDotPrinter.open
      plasticineCtrlDotPrinter.open
    }
  }

}
