package spade

package object codegen extends util.PrismAlias {
  type Spade = spade.Spade
  type SpadeDesign = spade.node.SpadeDesign
  type SpadeNode = spade.node.SpadeNode
  type Parameter = spade.node.Parameter
  type PinType = spade.node.PinType
  type Pin[P<:PinType] = spade.node.Pin[P]
  type Module = spade.node.Module
  type SpadePass = spade.pass.SpadePass
  type SpadeWorld = spade.pass.SpadeWorld
  type SpadeTraversal = spade.pass.SpadeTraversal
  type SpadeMapLike = spade.config.SpadeMapLike
  type SpadeMetadata = spade.util.SpadeMetadata
}
