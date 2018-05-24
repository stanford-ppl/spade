package spade
package node

trait MeshTopParam extends TopParam {
  val numRows:Int
  val numCols:Int
  val centrolPattern:GridCentrolPattern
  val fringePattern:GridFringePattern
}
