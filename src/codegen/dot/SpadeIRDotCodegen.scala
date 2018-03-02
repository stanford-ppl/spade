package spade.codegen
import spade._
import spade.node._

import prism._
import prism.codegen._

import sys.process._
import scala.reflect._

class SpadeIRDotCodegen[B<:BundleType:ClassTag](val fileName:String)(implicit design:Spade) extends SpadeCodegen with IRDotCodegen {

  import spademeta._

  //override def label(attr:DotAttr, n:Any) = {
    //var label = quote(n) 
    //attr.label(label)
  //}

  //def shape(attr:DotAttr, n:Any) = attr.shape(box)

  val scale = 20

  def pos(attr:DotAttr, n:Any) = {
    import design.topParam._
    indexOf.get(n.asInstanceOf[SpadeNode]).foreach { case List(x,y) =>
      val coord:Option[(Double, Double)] = n match {
        case n if isSCU(n) & ((x<0) | (x>=numCols)) => Some(x, y-0.2)
        case n if isPCU(n) & ((x<0) | (x>=numCols)) => Some(x, y-0.8)
        //case n:MemoryController => Some((x, y-0.5))
        //case n:OuterComputeUnit => Some((x-0.3, y-0.3))
        case n:SwitchBox => Some((x-0.5, y-0.5))
        case n:ArgFringe => None
        case n:CU => Some((x,y))
        case n => None
      }
      coord.foreach { case (x,y) => attr.pos((x*scale, y*scale)) }
    }
    attr
  }

  override def color(attr:DotAttr, n:Any) = n match {
    case n:CU if isPCU(n) => attr.fillcolor("dodgerblue").style(filled)
    case n:CU if isPMU(n) => attr.fillcolor("lightseagreen").style(filled)
    case n:CU if isSCU(n) => attr.fillcolor("palevioletred1").style(filled)
    case n:SwitchBox => attr.fillcolor("indianred1").style(filled)
    //case n:OuterComputeUnit => Color("orange")
    //case n:MemoryController => Color("forestgreen")
    case n => super.color(attr, n)
  }

  override def setAttrs(n:Any):DotAttr = {
    var attr = super.setAttrs(n)
    attr = pos(attr, n)
    attr
  }
  
  override def emitNode(n:N) = {
    n match {
      case n:Top => super.visitNode(n)
      case n:Routable => emitSingleNode(n)
      case n => super.emitNode(n) 
    }
  }

  override def emitEdge(from:prism.node.Edge[N], to:prism.node.Edge[N], attr:DotAttr):Unit = {
    (from, to) match {
      case (from, to) if is[B](from) & is[B](to) => emitEdgeMatched(from.src.asInstanceOf[N], to.src, attr) 
      case _ => dbg(s"${implicitly[ClassTag[B]]} ${bctOf(from)} ${bctOf(to)}")
    }
  }

}

