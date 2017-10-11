package spade.node

import spade._
import spade.util._

/* Routable element at interconnection level */
trait Routable extends Module with Simulatable {
  import spademeta._
  implicit val ctrler:this.type = this 
  def scalarIO:ScalarIO[this.type]
  def vectorIO:VectorIO[this.type]
  def ctrlIO:ControlIO[this.type]
  def gridIOs = scalarIO :: vectorIO :: ctrlIO :: Nil
  def cins = ctrlIO.ins // Control inputs
  def couts = ctrlIO.outs // Control outputs
  def sins = scalarIO.ins // Scalar Inputs
  def souts = scalarIO.outs // Scalar Outputs
  def vins = vectorIO.ins// Input Buses/Vector inputs
  def vouts = vectorIO.outs // Output Buses/Vector outputs
  def gins = cins ++ sins ++ vins
  def gouts = couts ++ souts ++ vouts

  def isMCU:Boolean = this.isInstanceOf[MemoryComputeUnit]
  def isSCU:Boolean = this.isInstanceOf[ScalarComputeUnit]
  def isTop:Boolean = this.isInstanceOf[Top]
  def asCU:ComputeUnit = this.asInstanceOf[ComputeUnit]
  def genConnections:this.type = { ConfigFactory.genConnections(this); this } 
  def config(implicit spade:SwitchNetwork):Unit = {}
  //override def toString = s"${coordOf.get(this).fold(super.toString) { case (x,y) => s"$typeStr[$x,$y]"}}"
}

