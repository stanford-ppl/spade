package spade
package param

import SpadeConfig._
case class DesignParam(
  wordWidth:Int = option[Int]("word"),
  vecWidth:Int = option[Int]("vec"),
  clockFrequency:Int = 1000000000, //Hz
  topParam:TopParam = defaultTopParam
) extends Parameter
