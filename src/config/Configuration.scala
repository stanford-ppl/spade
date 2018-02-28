package spade.config

import spade.util._

trait Configuration

trait Configurable {
  type CT <: Configuration
  def toConfig(x:Configuration) = x.asInstanceOf[CT]
}

case class DummyConfig() extends Configuration
