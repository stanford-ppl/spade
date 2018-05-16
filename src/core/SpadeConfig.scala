package spade

object SpadeConfig extends prism.GlobalConfig {
  register("save-spade", default=false, info="Save IR into a file")
  register("load-spade", default=false, info="Load IR from a file")
  register("simulate", false, info="Enable simulation")
  register("waveform", true, info="Enable waveform")
  register("time-out", 100, info="Simulation time out after X cycles")
  register("open", default=false, info="Open dot graph after codegen")

  var saveDesign:Boolean = option[Boolean]("save-spade")
  var loadDesign:Boolean = option[Boolean]("load-spade")

  // Properties go here
  var codegen:Boolean = Config.option[Boolean]("codegen")
  var simulate:Boolean = option[Boolean]("simulate")
  var verbose:Boolean = Config.option[Boolean]("verbose")
  var waveform:Boolean = option[Boolean]("waveform")
  var simulationTimeOut:Int = option[Int]("time-out")

  def debug = Config.option[Boolean]("debug")
  def openDot = option[Boolean]("open")
  
}
