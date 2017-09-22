package arch

import spade.network._
import spade.node._
import spade._

import pirc.enums._

object SN2x2Test extends Spade {
  override lazy val topParam = new TopParam(numRows=2, numCols=2, numArgIns=3, numArgOuts=3)

  override def ctrlNetwork = new CtrlNetwork {
    channelWidth("pos"->List("left", "right")) = 0
  }

  override def vectorNetwork = new VectorNetwork {
    channelWidth("pos"->List("left", "right")) = 0
  }

  override def scalarNetwork = new ScalarNetwork {
    channelWidth("pos"->List("left", "right")) = 0
  }
}

