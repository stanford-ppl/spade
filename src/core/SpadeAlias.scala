package spade

trait SpadeLocalAlias {
  type SpadePass = spade.pass.SpadePass
  type SpadeWorld = spade.pass.SpadeWorld
  type SpadeTraversal = spade.pass.SpadeTraversal

  type SpadeMapLike = spade.config.SpadeMapLike

  type SpadeMetadata = spade.util.SpadeMetadata
}

trait SpadeAlias extends SpadeLocalAlias {
  type Spade = spade.Spade
  type SNode = spade.node.SpadeNode

  type FIMap = spade.config.FIMap
  val FIMap = spade.config.FIMap
  type ConfigMap = spade.config.ConfigMap
  val ConfigMap = spade.config.ConfigMap
  val SpadeConfig = spade.SpadeConfig
}

