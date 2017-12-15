package spade.network
                          
import spade._
import spade.node._
import spade.util._

import pirc._
import pirc.util._
import pirc.enums._

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import pureconfig._
import java.io.File

// Common configuration generator 
object ConfigFactory extends Logger {

  case class PlasticineConf(
    scuSin:Int,
    scuSout:Int,
    scuStages:Int,
    scuRegs:Int,
    pcuVin:Int,
    pcuVout:Int,
    pcuSin:Int,
    pcuSout:Int,
    var pcuStages:Int, // Can be reset by PIRDSE
    pcuRegs:Int,
    pmuVin:Int,
    pmuVout:Int,
    pmuSin:Int,
    pmuSout:Int,
    var pmuStages:Int,
    pmuRegs:Int,
    lanes: Int,
    wordWidth: Int
  )

    lazy val defaultPlasticine = loadConfig[PlasticineConf](com.typesafe.config.ConfigFactory.parseString("""
plasticine {
  scu-sin = 8
  scu-sout = 2
  scu-stages = 5
  scu-regs = 16
  pcu-vin = 4
  pcu-vout = 2
  pcu-sin = 6
  pcu-sout = 2
  pcu-stages = 7
  pcu-regs = 16
  pmu-vin = 4
  pmu-vout = 1
  pmu-sin = 4
  pmu-sout = 1
  pmu-stages = 0
  pmu-regs = 16
  lanes = 16
  word-width = 32
}
"""), "plasticine").right.get

  lazy val plasticineConf = {
    val path = s"${Config.SPATIAL_HOME}/apps/resources/application.conf"
    val config = loadConfig[PlasticineConf](com.typesafe.config.ConfigFactory.parseFile(new File(path)), "plasticine") match {
      case Right(config) => 
        info(s"Loading configuration from $path")
        config
      case Left(failures) =>
        warn(s"Unable to load plasticine config from $path. Using default config")
        defaultPlasticine
    }
    config
  }

}
