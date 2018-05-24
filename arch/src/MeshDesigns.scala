package arch

import spade.node._
import prism.enums._

class StaticMeshCB(numRows:Int=2, numCols:Int=2, numArgIns:Int=3, numArgOuts:Int=3) extends Spade {
  override lazy val topParam = StaticMeshTopParam(
    numRows=numRows,
    numCols=numCols,
    centrolPattern=Checkerboard(),
    fringePattern=MCOnly(
      argFringeParam=ArgFringeParam(numArgIns=numArgIns, numArgOuts=numArgOuts)
    )
  )
}

object SMeshCB2x2 extends StaticMeshCB(numRows=2, numCols=2, numArgIns=3, numArgOuts=3)
object SMeshCB4x4 extends StaticMeshCB(numRows=4, numCols=4, numArgIns=10, numArgOuts=3)
object SMeshCB16x8 extends StaticMeshCB(numRows=16, numCols=8, numArgIns=10, numArgOuts=3)

class DynamicMeshCB(numRows:Int=2, numCols:Int=2, numArgIns:Int=3, numArgOuts:Int=3) extends Spade {
  override lazy val topParam = DynamicMeshTopParam(
    numRows=numRows,
    numCols=numCols,
    centrolPattern=Checkerboard(),
    fringePattern=MCOnly(
      argFringeParam=ArgFringeParam(numArgIns=numArgIns, numArgOuts=numArgOuts)
    )
  )
}

object DMeshCB2x2 extends DynamicMeshCB(numRows=2, numCols=2, numArgIns=3, numArgOuts=3)
object DMeshCB4x2 extends DynamicMeshCB(numRows=4, numCols=2, numArgIns=3, numArgOuts=3)
object DMeshCB4x4 extends DynamicMeshCB(numRows=4, numCols=4, numArgIns=10, numArgOuts=3)
object DMeshCB16x8 extends DynamicMeshCB(numRows=16, numCols=8, numArgIns=10, numArgOuts=3)

object MyDesign extends Spade {
  /* Example of overriding memory size parameters */
  //override lazy val topParam = DynamicMeshTopParam(
    //numRows=4,
    //numCols=4,
    //centrolPattern=Checkerboard(
      //pcuParam=PCUParam(
        //controlFifoParam=FIFOParam(5),
        //scalarFifoParam=FIFOParam(5),
        //vectorFifoParam=FIFOParam(5)
      //),
      //pmuParam=PMUParam(
        //controlFifoParam=FIFOParam(5),
        //scalarFifoParam=FIFOParam(5),
        //vectorFifoParam=FIFOParam(5),
        //sramParam=SRAMParam(size=256 * 1024 / 4, depth=4) // 256 kB capacity
      //)
    //),
    //fringePattern=MC_DramAG(
      //argFringeParam=ArgFringeParam(numArgIns=3, numArgOuts=3),
      //mcParam=MCParam(
        //wOffsetFifoParam=FIFOParam(size=16),
        //rOffsetFifoParam=FIFOParam(size=16),
        //wSizeFifoParam=FIFOParam(size=16),
        //rSizeFifoParam=FIFOParam(size=16),
        //sDataFifoParam=FIFOParam(size=16),
        //vDataFifoParam=FIFOParam(size=16)
      //)
    //)
  //)
  override lazy val topParam = StaticCMeshTopParam(
    numRows=4,
    numCols=4,
    pattern=CMeshCheckerboard()
  )
}
