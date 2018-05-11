package arch

import spade.node._
import prism.enums._

object Asic extends Spade {
  override lazy val topParam = AsicTopParam()
}
