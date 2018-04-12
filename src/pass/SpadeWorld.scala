package spade.pass

trait SpadeWorld {
  implicit val nct = classTag[N]
  type N = SpadeNode
  type P = Module
  type A = Pin[_]
  type D = SpadeDesign
}

