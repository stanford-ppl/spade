package spade.codegen

import spade.node._ 

import sys.process._

class NetworkDotCodegen[B<:PinType:ClassTag](val fileName:String)(implicit compiler:Spade) extends SpadeCodegen with IRDotCodegen {

  import spademeta._

  def getLabel(n:Any) = quote(n)

  def labelWithPort(attr:DotAttr, n:Routable) = {
    val nio = n.nios.flatMap(as[Bundle,B]).head.asInstanceOf[GridBundle[B]]
    val recs = ListBuffer[String]()
    def ports(dir:String) = {
      var ins = nio.inAt(dir).map{io => s"<$io> $io(${indexOf(io)})"}
      var outs = nio.outAt(dir).map{io => s"<$io> $io(${indexOf(io)})"}
      val maxLength = Math.max(ins.size, outs.size)
      ins = ins ++ List.fill(maxLength-ins.size){""}
      outs = outs ++ List.fill(maxLength-outs.size){""}
      val ios = ins.zip(outs).flatMap{case (i,o) => 
        if (dir=="S" || dir=="E") List(o,i)
        else List(i,o)
      }
      ios.mkString("|")
    }
    recs += s"{${ports("NW")}  | ${ports("N")}          | ${ports("NE")}}"
    recs += s"{{${ports("W")}} | {${getLabel(n)}}  | {${ports("E")}}}"
    recs += s"{${ports("SW")}  | ${ports("S")}          | ${ports("SE")}}"
    val label = s"{${recs.mkString("|")}}"
    attr.label(label)
  }

  //def shape(attr:DotAttr, n:Any) = attr.shape(box)

  val scale = 5

  def pos(attr:DotAttr, n:Any) = {
    val dynamic = isDynamic(compiler.top)
    (n, compiler.topParam) match {
      case (n:SpadeNode, param:MeshTopParam) =>
        indexOf.get(n).foreach { case List(x,y) =>
          val coord:Option[(Double, Double)] = n match {
            //case n:SCU if ((x<0) | (x>=numCols)) => Some(x, y-0.2)
            //case n:PCU if ((x<0) | (x>=numCols)) => Some(x, y-0.8)
            //case n:MC => Some((x, y-0.5))
            //case n:OuterComputeUnit => Some((x-0.3, y-0.3))
            //case n:SwitchBox => Some((x-0.5, y-0.5))
            case n:ArgFringe => None
            case n => Some((x,y))
          }
          coord.foreach { case (x,y) => 
            n match {
              case n:CU if dynamic => attr.pos(((x+0.5)*scale, (y + 0.5)*scale))
              case n => attr.pos((x*scale, y*scale))
            }
          }
        }
      case ((n:ArgFringe, "top"), param:MeshTopParam) if dynamic => attr.pos(((param.numCols/2)*scale, (param.numRows-1)*scale+scale))
      case ((n:ArgFringe, "bottom"), param:MeshTopParam) if dynamic => attr.pos(((param.numCols/2)*scale, -scale))
      case ((n:ArgFringe, "top"), param:MeshTopParam) => attr.pos(((param.numCols/2)*scale*2, (param.numRows+1)*scale*2))
      case ((n:ArgFringe, "bottom"), param:MeshTopParam) => attr.pos(((param.numCols/2)*scale*2, -scale*2))
    }
    attr
  }

  override def color(attr:DotAttr, n:Any) = n match {
    case n:PCU => attr.fillcolor("dodgerblue").style(filled)
    case n:PMU => attr.fillcolor("lightseagreen").style(filled)
    case n:SCU => attr.fillcolor("palevioletred1").style(filled)
    case n:MC => attr.fillcolor("forestgreen").style(filled)
    case n:SwitchBox => attr.fillcolor("indianred1").style(filled)
    case (n:ArgFringe, "top") => attr.fillcolor("indianred1").style(filled)
    case (n:ArgFringe, "bottom") => attr.fillcolor("indianred1").style(filled)
    //case n:OuterComputeUnit => Color("orange")
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
      case n:ArgFringe =>
        emitNode(s"${n}_top",setAttrs((n, "top")))
        emitNode(s"${n}_bottom",setAttrs((n, "bottom")))
        nodes += n
      case n:Routable => emitSingleNode(n)
    }
  }

  override def emitEdge(from:prism.node.Edge[N], to:prism.node.Edge[N], attr:DotAttr):Unit = {
    (from, to) match {
      case (from:DirectedEdge[_,_], to:DirectedEdge[_,_]) if is[B](from) & is[B](to) => emitEdgeMatched(from.src.asInstanceOf[N], to.src, attr) 
      case _ => 
    }
  }

  override def emitEdge(from:N, to:N, attr:DotAttr):Unit = {
    val (fromStr, toStr) = (from, to) match {
      case (from:SwitchBox, to:ArgFringe) =>
        val List(x,y) = indexOf(from)
        val toStr = if (y==0) s"${to}_bottom" else s"${to}_top"
        (from.toString, toStr)
      case (from:ArgFringe, to:SwitchBox) =>
        val List(x,y) = indexOf(to)
        val fromStr = if (y==0) s"${from}_bottom" else s"${from}_top"
        (fromStr, to.toString)
      case (from, to) => (from.toString, to.toString)
    }
    super.emitEdge(fromStr, toStr, attr) // String, String
  }

}

