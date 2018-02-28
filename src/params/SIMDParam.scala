package spade.params

import prism.enums._
import prism.node._

import spade.node._

case class DefaultSIMDParam (
  numStages:Int,
  numLanes:Int,
  numRegs:Int
) extends SIMDParam {
  val numReductionStages = (Math.log(numLanes) / Math.log(2)).toInt
  val numNonReductionStages = numStages - numReductionStages
  val reductionIndices = List.tabulate(numStages){ i =>
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
  val numLanes:Int
  val numRegs:Int
  val reductionIndices:List[Option[Int]]
  def set(cu:CU):Unit
  val pipeRegParams = List.tabulate(numRegs) { ir => PipeRegParam() }
  lazy val stageParams = reductionIndices.map { reductionIdx =>
    StageParam(pipeRegParams=pipeRegParams, reductionIdx=reductionIdx)
  }
}

