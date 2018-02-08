package spade.codegen

import spade._
import spade.node._
import spade.util._

import pirc._
import pirc.codegen._
import pirc.util._

import scala.collection.mutable.ListBuffer
import sys.process._
import scala.language.postfixOps
import scala.language.existentials

abstract class PlasticineDotGen(fn:String)(implicit design:Spade) extends Codegen with DotCodegen {
  import spademeta._

  def shouldRun = Config.debug

  private var _mapping:Option[SpadeMap] = None
  def mapping:Option[SpadeMap] = _mapping

  val scale:Int

  def io(prt:Routable):GridIO[_<:PortType, Routable]

  override lazy val stream = newStream(fn)

  trait Mode
  object OnlyOCU extends Mode
  object AllCU extends Mode
  object NoOCU extends Mode

  val mode:Mode = AllCU 
  //val mode:Mode = OnlyOCU 
  
  def linkColor = Color("indianred1") 

  def color(prt:Routable):Color = prt match {
    case pscu:ScalarComputeUnit => Color("palevioletred1")
    case ppmu:PatternMemoryUnit => Color("lightseagreen")
    //case pmu:PMU => Color("lightseagreen")
    case pocu:OuterComputeUnit => Color("orange")
    case pcu:PatternComputeUnit => Color("dodgerblue")
    case pmc:MemoryController => Color("forestgreen")
    case psb:SwitchBox => linkColor 
    case ptop:Top => Color("indianred1")
  }

  override def quote(n:Any):String = {
    n match {
      case (n,b) =>
        val bottom = b.asInstanceOf[Boolean]
        n match {
          case ptop:Top if (bottom) => s"""${quote(ptop)}_bottom"""
          case ptop:Top if (!bottom) => s"""${quote(ptop)}_top"""
        }
      case n:Node => spade.util.quote(n)
    }
  }

  def print:this.type = {
    emitBlock("digraph G") {
      val prts = mode match {
        case NoOCU =>
          design.top.prts.filterNot{_.isInstanceOf[OuterComputeUnit]}
        case OnlyOCU =>
          design.top.prts.filter{ prt => prt.isInstanceOf[ScalarComputeUnit] || prt.isInstanceOf[OuterComputeUnit]}
        case AllCU =>
          design.top.prts
      }
      prts.foreach { prt =>
        emitRoutables(prt)
        val ins =  mode match {
          case NoOCU =>
            io(prt).ins.filterNot{ in => in.fanIns.head.src.isInstanceOf[OuterComputeUnit]}
          case OnlyOCU =>
            io(prt).ins.filter{ in => in.fanIns.head.src.isInstanceOf[ScalarComputeUnit] || in.fanIns.head.src.isInstanceOf[OuterComputeUnit]}
          case AllCU =>
            io(prt).ins
        }
        ins.foreach { in => emitInput(in) }
      }
    }
    close
    this
  }

  def print(mapping:Option[SpadeMap]):this.type = {
    this._mapping = mapping
    print
    this
  }

  def open = {
    s"out/bin/run -c ${getPath} &".replace(".dot", "") !
  }

  /*
   * Position of Controller / SwitchBox as a function of coordinate
   * */
  def setPos(prt:Routable, attr:DotAttr) = {
    import design.topParam._

    coordOf.get(prt).foreach { case (x,y) =>
      val coord:Option[(Double, Double)] = prt match {
        case pscu:ScalarComputeUnit if (x<0) | (x>=numCols) => Some(x, y-0.2)
        case ppcu:PatternComputeUnit if (x<0) | (x>=numCols) => Some(x, y-0.8)
        case pmc:MemoryController => Some((x, y-0.5))
        case pocu:OuterComputeUnit => Some((x-0.3, y-0.3))
        case psb:SwitchBox => Some((x-0.5, y-0.5))
        case ptop:Top => None
        case pcu => Some((x, y))
      }
      coord.foreach { case (x,y) => attr.pos((x*scale, y*scale)) }
    }

  }

  def setColor(prt:Routable, attr:DotAttr) = {
    // Color node if any of the inputs is mapped
    mapping.foreach { mp => 
      prt match {
        case prt:Controller =>
          if (mp.cfmap.contains(prt) || io(prt).ins.exists( in => mp.fimap.contains(in)) || io(prt).outs.exists(out => mp.fimap.contains(out)))
            attr.style(filled).fillcolor(color(prt))
        case prt:SwitchBox =>
          if (io(prt).ins.exists(in => mp.fimap.contains(in)))
            attr.style(filled).fillcolor(color(prt))
      }
    }
  }

  def setColor(pin:GlobalInput[_<:PortType, Module], pout:GlobalOutput[_<:PortType, Module], attr:DotAttr) = {
    mapping.foreach { m => 
      if (m.fimap.get(pin).fold(false){ _ == pout }) {
        attr.color(linkColor).style(bold)
      }
    }
  }

  def getLabel(prt:Routable):String = quote(prt)

  def setLabel(pin:GlobalInput[_<:PortType, Module], pout:GlobalOutput[_<:PortType,Module], attr:DotAttr) = { }

  def setLabel(prt:Routable, attr:DotAttr) = {
    val recs = ListBuffer[String]()
    prt match {
      case ptop:Top => recs += s"$ptop" 
      case pcl:Controller => 
        def ports(dir:String) = {
          var ins = io(pcl).inAt(dir).map{io => s"<$io> $io(${indexOf(io)})"}
          var outs = io(pcl).outAt(dir).map{io => s"<$io> $io(${indexOf(io)})"}
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
        recs += s"{{${ports("W")}} | {${getLabel(pcl)}}  | {${ports("E")}}}"
        recs += s"{${ports("SW")}  | ${ports("S")}          | ${ports("SE")}}"
      case psb:SwitchBox => 
        recs += getLabel(prt)
    }
    val label = s"{${recs.mkString("|")}}"
    attr.label(label)
  }

  def emitRoutables(prt:Routable) = {
    import design.topParam._

    var attr = DotAttr().shape(Mrecord)
    setLabel(prt, attr)
    setPos(prt, attr)
    setColor(prt, attr)
    prt match {
      case ptop:Top => s"$ptop" 
        emitNode(quote(ptop, false), DotAttr.copy(attr).pos( (numCols/2-1)*scale+scale/2, numRows*scale))
        emitNode(quote(ptop, true), DotAttr.copy(attr).pos( (numCols/2-1)*scale+scale/2, -scale))
      case _ =>
        emitNode(prt, attr)
    }
  }

  def emitInput(pin:GlobalInput[_<:PortType,Routable]) = {
    val prt:Routable = pin.src
    pin.fanIns.foreach { po =>
      val pout = po.asGlobal
      val attr = DotAttr()
      val to = pin.src match {
        case psb:SwitchBox => s"$psb"
        case ptop:Top => quote(ptop, coordOf(pout.src)._2==0)
        case _ => s"$prt:$pin"
      }
      val from = pout.src match {
        case from:SwitchBox =>
          attr.label.foreach { l => attr.label(l + s"\n(o-${indexOf(pout)})") }
          s"$from"
        case from:Top =>
          val bottom = coordOf(pin.src)._2==0 
          quote(from, bottom)
        case from => s"$from:$pout"
      }
      setLabel(pin, pout, attr)
      setColor(pin, pout, attr)
      emitEdge(from, to, attr)
    }
  }

  override def runPass = {
    print
  }

  override def finPass = {
    close
    endInfo(s"Finishing $name in ${getPath}...")
  }

}

class PlasticineCtrlDotPrinter(file:String)(implicit design:Spade)
  extends PlasticineDotGen(file) { 
  def this()(implicit design:Spade) = this(SpadeConfig.spadeCtrlNetwork)

  val scale = 20

  def io(prt:Routable) = prt.ctrlIO
}

class PlasticineScalarDotPrinter(file:String)(implicit design:Spade) 
  extends PlasticineDotGen(file) { 
  def this()(implicit design:Spade) = this(SpadeConfig.spadeScalarNetwork)
  
  val scale = 20

  def io(prt:Routable) = prt.scalarIO

}

class PlasticineVectorDotPrinter(file:String)(implicit design:Spade) 
  extends PlasticineDotGen(file) { 
  def this()(implicit design:Spade) = this(SpadeConfig.spadeVectorNetwork)
  
  val scale = 15

  def io(prt:Routable) = prt.vectorIO
}
