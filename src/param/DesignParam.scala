package spade
package param

import SpadeConfig._
case class DesignParam(
  wordWidth:Int = option[Int]("word"),
  vecWidth:Int = option[Int]("vec"),
  clockFrequency:Int = 1000000000, //Hz
  burstSize:Int = 512, // bit
  topParam:TopParam = defaultTopParam
) extends Parameter {
  def burstSizeWord = burstSize / wordWidth
  def burstSizeByte = burstSize / 8 
}
