package spade
package param

trait TopParam extends Parameter {
  lazy val wordWidth = 32
  lazy val vecWidth = 16
  lazy val clockFrequency:Int = 1000000000 //Hz
  lazy val burstSize:Int = 512 // bit
  val busWithReady:Boolean // TODO: make this a parameter of network
  def burstSizeWord = burstSize / wordWidth
  def burstSizeByte = burstSize / 8 
}
