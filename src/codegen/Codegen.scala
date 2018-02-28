package spade.codegen

import spade._

abstract class Codegen(implicit design:Spade) extends SpadePass with prism.codegen.Codegen
