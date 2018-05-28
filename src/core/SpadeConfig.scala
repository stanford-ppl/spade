package spade

object SpadeConfig extends prism.GlobalConfig {
  register("save-spade", default=false, info="Save IR into a file")
  register("load-spade", default=false, info="Load IR from a file")
  register("simulate", false, info="Enable simulation")
  register("waveform", true, info="Enable waveform")
  register("time-out", 100, info="Simulation time out after X cycles")
  register("open", default=false, info="Open dot graph after codegen")

  def saveDesign:Boolean = option[Boolean]("save-spade")
  def loadDesign:Boolean = option[Boolean]("load-spade")

  def codegen:Boolean = Config.option[Boolean]("codegen")
  def simulate:Boolean = option[Boolean]("simulate")
  def verbose:Boolean = Config.option[Boolean]("verbose")
  def waveform:Boolean = option[Boolean]("waveform")
  def simulationTimeOut:Int = option[Int]("time-out")

  def debug = Config.option[Boolean]("debug")
  def openDot = option[Boolean]("open")
  
  /* Architecture parameters */
  register[Int]("word", default=32, info="Word width")
  register[Int]("vec", default=16, info="Vector width")
  register[Int]("row", default=2, info="number of rows")
  register[Int]("col", default=2, info="number of columns")
  register[String]("net", default="static", info="dynamic or static")
  register[String]("topo", default="mesh", info="mesh, torus, or cmesh")
  register[Boolean]("nn", default=false, info="nearest neighbor")
  register[Boolean]("dag", default=true, info="has dram address generator")
  register[String]("pattern", default="checkerboard", info="mesh/torus=checkerboard,cstrip,rstrip,mixall,half&half, cmesh=checkerboard")
  register[Int]("argin", default=6, info="number of ArgIn")
  register[Int]("argout", default=4, info="number of ArgOut")
  register[Int]("tokenout", default=5, info="number of TokenOut")
}
