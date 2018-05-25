package spade
package param

case class DesignParam(
  wordWidth:Int = 32,
  vecWidth:Int = 16,
  clockFrequency:Int = 1000000000, //Hz
  topParam:TopParam=StaticGridTopParam(),
) extends Parameter