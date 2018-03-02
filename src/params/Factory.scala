package spade.params

import spade._
import spade.node._
import spade.network._

import prism.node._
import prism.enums._
import prism.util._
import prism._

import scala.language.reflectiveCalls
import scala.reflect._

import scala.collection.mutable._
import pureconfig._
import java.io.File

object Factory {
  def create(param:Any)(implicit design:SpadeDesign) = param match {
    case param:MeshDesignParam => MeshTop(param)
  }
  def create(param:Any, nios:List[NetworkBundle[_]])(implicit design:SpadeDesign) = param match {
    case param:CUParam => CU(param, nios)
  }

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
