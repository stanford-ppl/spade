package arch

import spade._
import spade.params._

import prism.enums._

import scala.language.implicitConversions
import scala.language.reflectiveCalls

class MeshCB(numRows:Int=2, numCols:Int=2, numArgIns:Int=3, numArgOuts:Int=3) extends Spade {
  override def toString = s"SN${numRows}x${numCols}"

  override lazy val topParam = MeshDesignParam(
    numRows=numRows,
    numCols=numCols,
    pattern=Checkerboard(),
    argFringeParam=ArgFringeParam(numArgIns=numArgIns, numArgOuts=numArgOuts)
  )()
}

object MeshCB2x2 extends MeshCB(numRows=2, numCols=2, numArgIns=3, numArgOuts=3)
