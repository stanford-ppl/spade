package spade

package object pass extends spade.util.PrismAlias {
  type Spade = spade.Spade
  type SpadeDesign = spade.node.SpadeDesign
  type SpadeNode = spade.node.SpadeNode
  type Module = spade.node.Module
  type PinType = spade.node.PinType
  type Pin[P<:PinType] = spade.node.Pin[P]
  type SpadeMapLike = spade.config.SpadeMapLike
  type SpadeMetadata = spade.util.SpadeMetadata
}
