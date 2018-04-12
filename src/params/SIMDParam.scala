package spade.node

import prism.enums._

case class DefaultSIMDParam (
  numStages:Int,
  vectorized:Boolean,
  numRegs:Int
) extends SIMDParam {
  lazy val numReductionStages = (Math.log(numLanes) / Math.log(2)).toInt
  lazy val numNonReductionStages = numStages - numReductionStages
  lazy val reductionIndices = List.tabulate(numStages){ i =>
    if (i >= numNonReductionStages) Some(i - numNonReductionStages) else None
  }
  def set(cu:CU):Unit = {
    import cu.param._
    pipeRegParams.slice(0, numCtrs).map(_.color(CounterReg))
    if (numReductionStages > 0) {
      pipeRegParams(0).color(ReduceReg) 
      pipeRegParams(1).color(AccumReg) 
    }
    pipeRegParams.takeRight(numScalarFifos).map(_.color(ScalarInReg))
    pipeRegParams.takeRight(numSouts).map(_.color(ScalarOutReg))
    pipeRegParams.takeRight(numVectorFifos).map(_.color(VecInReg))
    pipeRegParams.takeRight(numVouts).map(_.color(VecOutReg))
  }
}

trait SIMDParam extends Parameter {
  val numRegs:Int
  val vectorized:Boolean
  lazy val meshTopParam = collectOut[MeshTopParam]().head
  lazy val vecWidth = meshTopParam.vecWidth
  lazy val numLanes:Int = if (vectorized) vecWidth else 1
  val reductionIndices:List[Option[Int]]
  def set(cu:CU):Unit
  val pipeRegParams:List[PipeRegParam] = List.tabulate(numRegs) { ir => addField(PipeRegParam()) }
  lazy val stageParams = reductionIndices.map { reductionIdx =>
    addField(StageParam(reductionIdx=reductionIdx))
  }
}

