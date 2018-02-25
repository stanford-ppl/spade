package spade

import spade.node._
import spade.traversal.HiearchicalTraversal

import pirc._

import scala.language.existentials
import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}

package object util extends HiearchicalTraversal {

  /* ------------- Alias ------------- **/

  /* node */
  type Node = spade.node.Node
  type Module = spade.node.Module
  //type PortType = spade.node.PortType
  //type IO[P<:PortType, +S<:Module] = spade.node.IO[P,S]
  
  /* config */
  type Configurable = spade.config.Configurable
  type Configuration = spade.config.Configuration

  /* ------------- Alias (END) ------- **/

  implicit def pr_to_ip(pr:PipeReg):Input[Bus, PipeReg] = pr.in
  implicit def pr_to_op(pr:PipeReg):Output[Bus, PipeReg] = pr.out

  def quote(n:Any)(implicit spade:Spade):String = {
    val spademeta: SpadeMetadata = spade
    import spademeta._
    n match {
      case n:Iterable[_] => 
        s"[${n.map(quote).mkString(",")}]"
      case n:Routable => coordOf.get(n).fold(s"$n") { case (x,y) => s"$n[$x,$y]" }
      case n:GlobalIO[_,_] => s"${quote(n.src)}.$n[${n.index}]"
      case n:Node => indexOf.get(n).fold(s"$n"){ i =>s"$n[$i]"}
      case n => n.toString
    }
  }

  def isMapped(node:Node)(implicit mp: SpadeMapLike):Boolean = {
    node match {
      case n:Primitive if !isMapped(n.prt) => return false
      case n =>
    }
    node match {
      case n:DRAM => true
      case n:Controller => mp.cfmap.isMapped(n)
      case n:FuncUnit => isMapped(n.stage)
      case n:PipeReg => isMapped(n.in)
      case n:UpDownCounter => 
        n.ctrlBox match {
          case None => true
          case Some(cb:OuterCtrlBox) if cb.udsm == n.parent.get => true
          case Some(cb:CtrlBox) => mp.cfmap.isMapped(n)
        }
      case n:Input[_,_] => mp.fimap.contains(n) || n.fanIns.size==1
      case n:Output[_,_] => mp.fimap.contains(n)
      case n:SwitchBox => n.ios.exists(isMapped)
      case n:CtrlBox => isMapped(n.prt)
      case n:UpDownSM => isMapped(n.prt)
      case n:Const[_] => mp.cfmap.isMapped(n)
      case n:BroadCast[_] => isMapped(n.in) 
      case n:Slice[_,_] => isMapped(n.in) 
      case n:Delay[_] => isMapped(n.prt)
      case n:AndTree => n.ins.exists(isMapped)
      case n:AndGate => n.ins.exists(isMapped)
      case n:PredicateUnit => isMapped(n.in) 
      case n:Primitive => mp.cfmap.isMapped(n)
      case n => throw PIRException(s"Don't know how to check whether $n is mapped")
    }
  }

  def fanInOf[P<:PortType](in:Input[P,Module])(implicit mp:SpadeMapLike):Option[Output[P,Module]] = {
    mp.fimap.get(in).fold { 
      if (in.fanIns.size==1) Some(in.fanIns.head) else None
    } { out =>
      Some(out.asInstanceOf[Output[P, Module]])
    }
  }

  def fanOutOf[P<:PortType](out:Output[P,Module])(implicit mp:SpadeMapLike):List[Input[P,Module]] = {
    mp.fimap.get(out).fold { 
      if (out.fanOuts.size==1) List(out.fanOuts.head) else Nil
    } { ins =>
      ins.map { _.asInstanceOf[Input[P, Module]] }.toList
    }
  }

  def regsOf(x:Any):Set[ArchReg] = {
    def vi(x:Any):Iterable[Any] = x match {
      case x: Counter => Set() 
      case x: FuncUnit => Set() 
      case x => visitIn(x) 
    }
    def vo(x:Any):Iterable[Any] = x match {
      case x: Counter => Set() 
      case x: FuncUnit => Set() 
      case x => visitOut(x) 
    }
    x match {
      case x:Input[_,_] => collectIn[PipeReg](x, visitFunc=vi _).map{ _.reg }
      case x:Output[_,_] => collectOut[PipeReg](x, visitFunc=vo _).map{ _.reg }
      case _ => Set[ArchReg]()
    }
  }

  def zip[T1, T2, T](x1:Option[T1], x2:Option[T2])(lambda:(T1,T2) => T):Option[T] = (x1, x2) match {
    case (Some(x1), Some(x2)) => Some(lambda(x1, x2))
    case _ => None
  }
  def zip[T1, T2, T3, T](x1:Option[T1], x2:Option[T2], x3:Option[T3])(lambda:(T1,T2,T3) => T):Option[T] = (x1, x2, x3) match {
    case (Some(x1), Some(x2), Some(x3)) => Some(lambda(x1, x2, x3))
    case _ => None
  }
  def zip[T1, T2, T3, T4, T](x1:Option[T1], x2:Option[T2], x3:Option[T3], x4:Option[T4])(lambda:(T1,T2,T3,T4) => T):Option[T] = (x1, x2, x3, x4) match {
    case (Some(x1), Some(x2), Some(x3), Some(x4)) => Some(lambda(x1, x2, x3, x4))
    case _ => None
  }
}
