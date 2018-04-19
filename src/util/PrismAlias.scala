package spade.util

trait PrismAlias extends prism.util.Misc with prism.mapper.MappingUtil with prism.enums.Ops with prism.util.ScalaAlias with prism.util.FileManager {
  type Pass = prism.Pass
  type Compiler = prism.Compiler
  type Design = prism.node.Design
  type Node[N<:Node[N]] = prism.node.Node[N]
  type SubGraph[N<:Node[N]] = prism.node.SubGraph[N]
  type Atom[N<:Node[N]] = prism.node.Atom[N]
  type Codegen = prism.codegen.Codegen
  type Printer = prism.codegen.Printer
  type Logging = prism.codegen.Logging
  type IRPrinter = prism.codegen.IRPrinter
  type IRDotCodegen = prism.codegen.IRDotCodegen
  type ScalaCodegen = prism.codegen.ScalaCodegen
  type PIRException = prism.exceptions.PIRException
  type MappingFailure = prism.mapper.MappingFailure
  val PIRException = prism.exceptions.PIRException
  val Config = prism.Config
  val ConsoleLogger=prism.util.ConsoleLogger

  type Op = prism.enums.Op
}
