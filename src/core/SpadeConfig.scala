package spade

import prism._

import scala.collection.mutable

object SpadeConfig extends GlobalConfig {

  var saveDesign:Boolean = register("save-spade", default=false, info="Save IR into a file") { saveDesign = _ }
  var loadDesign:Boolean = register("load-spade", default=false, info="Load IR from a file") { loadDesign = _ }

  // Properties go here
  var test:Boolean = register("test", false) { test = _ }
  var codegen:Boolean = register("codegen", false) { codegen = _ }
  var genDot:Boolean = register("dot", true) { genDot = _ }
  var simulate:Boolean = register("simulate", false) { simulate = _ }
  var verbose:Boolean = register("verbose", false) { verbose = _ }
  var waveform:Boolean = register("waveform", true) { waveform = _ }
  var simulationTimeOut:Int = register("time-out", 100) { simulationTimeOut = _ }

  var spadeFile:String = register("spadefile", "Spade.log") { spadeFile = _ }
  //var pisaFile = getProperty("pir.pisafile", "pisa.json")
  var spadeVectorNetwork:String = register("spade_vector_network", "VecNetwork.dot") { spadeVectorNetwork = _ }
  var spadeScalarNetwork:String = register("spade_scalar_network", "ScalNetwork.dot") { spadeScalarNetwork = _ }
  var spadeCtrlNetwork:String = register("spade_ctrl_network", "CtrlNetwork.dot") { spadeCtrlNetwork = _ }
  var spadeArgInOut:String = register("spade_arginout", "ArgInOut.dot") { spadeArgInOut = _ }
  var spadeCtr:String = register("spade_ctr", "PCtr.dot") { spadeCtr = _ }

  def debug = Config.debug
  var debugCodegen:Boolean = debug && register("debug-codegen", true) { debugCodegen = _ }
  var openDot:Boolean = register("open", false) { openDot = _ }
  
}
