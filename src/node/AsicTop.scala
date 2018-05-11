package spade
package node

case class AsicTop(override val param:AsicTopParam)(implicit design:SpadeDesign) extends Top(param)
