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

// Common configuration generator 
object ConfigFactory extends Logger {

  case class PlasticineConf(
    sinUcu: Int,
    stagesUcu: Int,
    sinPcu: Int,
    soutPcu:Int,
    vinPcu: Int,
    voutPcu: Int,
    regsPcu: Int,
    comp: Int,
    sinPmu: Int,
    soutPmu:Int,
    vinPmu: Int,
    voutPmu: Int,
    regsPmu: Int,
    rw: Int,
    lanes: Int
  )

    val defaultPlasticine =  com.typesafe.config.ConfigFactory.parseString("""
plasticine {
  sin-ucu = 10
  stages-ucu = 10
  sin-pcu = 10
  sout-pcu = 10
  vin-pcu = 4
  vout-pcu = 1
  regs-pcu = 16
  comp = 10
  sin-pmu = 10
  sout-pmu = 10
  vin-pmu = 4
  vout-pmu = 1
  regs-pmu = 16
  rw = 10
  lanes = 16
}
  """)

  lazy val mergedPlasticineConf = com.typesafe.config.ConfigFactory.load().withFallback(defaultPlasticine).resolve()

  lazy val plasticineConf = loadConfig[PlasticineConf](mergedPlasticineConf, "plasticine") match {
    case Right(config) => 
      config
    case Left(failures) =>
      throw PIRException(s"Unable to load plasticine config!")
  }

}
