package spade.network
                          
import spade.node._
import spade._
import pirc.enums._

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.reflect.runtime.universe._

sealed trait Pattern {
  def cuAt(top:Top)(i:Int, j:Int):ComputeUnit
}
case object Checkerboard extends Pattern {
  def cuAt(top:Top)(i:Int, j:Int):ComputeUnit = {
    if ((i+j) % 2 == 0) top.pcuAt(i,j) 
    else top.pmuAt(i,j) 
  }
}
case object MixAll extends Pattern {
  def cuAt(top:Top)(i:Int, j:Int):ComputeUnit = {
    if (i % 2 == 0) {
      if (j % 2 == 0) top.pcuAt(i,j)
      else top.pmuAt(i,j)
    }
    else top.scuAt(i,j)
  }
}
case object HalfHalf extends Pattern {
  def cuAt(top:Top)(i:Int, j:Int):ComputeUnit = {
    if (i % 2 == 0) {
      if (j % 2 == 0) top.pcuAt(i,j) else top.pmuAt(i,j)
    } else {
      if (j % 2 == 0) top.pmuAt(i,j) else top.scuAt(i,j)
    }
  }
}
