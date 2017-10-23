package spade.network

import spade._
import spade.node._
import spade.network._

import pirc.enums._

import scala.language.implicitConversions
import scala.language.existentials

trait SwitchNetwork { self:Spade =>

  lazy val topParam = new TopParam()

  def pcuAt(x:Int, y:Int) = {
    import topParam._
    if (x>=0 && x<numCols) new PatternComputeUnitParam()
    else new SRAMAddrGenParam()
  }

  def pmuAt(x:Int, y:Int) = new PatternMemoryUnitParam()

  def scuAt(x:Int, y:Int) = new ScalarComputeUnitParam()

  def mcAt(x:Int, y:Int) = new MemoryControllerParam()

  def ocuAt(x:Int, y:Int) = new OuterComputeUnitParam()

  /* --- Network --- */
  def ctrlNetwork:GridNetwork = new CtrlNetwork()

  def vectorNetwork:GridNetwork = new VectorNetwork()

  def scalarNetwork:GridNetwork = new ScalarNetwork()


  /* --- alias to controllers */
  def cus = top.cus
  def pmus = top.pmus
  def pcus = top.pcus
  def scus = top.scus
  def ocus = top.ocus
  def prts = top.prts
  def mcs = top.mcs
  def ctrlers = top.ctrlers
  def sbs = top.sbs

  val cuArray = top.cuArray
  val dramAGs = top.dramAGs 
  val sramAGs = top.sramAGs 
  val mcArray = top.mcArray 
  val sbArray = top.sbArray
  val ocuArray = top.ocuArray 
}
