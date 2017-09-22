package spade.codegen

import spade._
import spade.pass.Pass

abstract class Codegen(implicit design:Spade) extends Pass with pirc.codegen.Codegen
