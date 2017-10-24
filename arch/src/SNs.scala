package arch

import spade.network._
import spade.node._
import spade._

import pirc.enums._

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.reflect.runtime.universe._

class SN(numRows:Int=2, numCols:Int=2, numArgIns:Int=3, numArgOuts:Int=3, pattern:Pattern=MixAll) extends Spade {
  override def toString = s"SN${numRows}x${numCols}"
  override lazy val topParam = new TopParam(
    numRows=numRows, 
    numCols=numCols, 
    numArgIns=numArgIns, 
    numArgOuts=numArgOuts, 
    pattern=pattern
  )
}

object SN1x1 extends SN(numRows=1, numCols=1, numArgIns=3, numArgOuts=3, pattern=Checkerboard) 
object SN1x2 extends SN(numRows=1, numCols=2, numArgIns=3, numArgOuts=3, pattern=Checkerboard) 
object SN2x2 extends SN(numRows=2, numCols=2, numArgIns=3, numArgOuts=3, pattern=Checkerboard) 
object SN2x3 extends SN(numRows=2, numCols=3, numArgIns=3, numArgOuts=3, pattern=Checkerboard) 
object SN4x4 extends SN(numRows=4, numCols=4, numArgIns=5, numArgOuts=3, pattern=Checkerboard) 
object SN8x8 extends SN(numRows=8, numCols=8, numArgIns=5, numArgOuts=3, pattern=Checkerboard) 
