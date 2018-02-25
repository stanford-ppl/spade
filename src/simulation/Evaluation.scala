package spade.simulation

import spade._
import spade.util._

import pirc.enums._
import pirc.exceptions._

import scala.language.implicitConversions

trait Evaluation {

  //class CurryHelper[T, Res](f: Seq[T] => Res, as: Seq[T]) {
    //def myCurry() = this
    //def apply(ts: T*) = new CurryHelper(f, as ++ ts)
    //def apply(ts: Seq[T]) = f(as ++ ts)
  //}
  //implicit def toCurryHelper[T, Res](f: Seq[T] => Res) = new CurryHelper(f, IndexedSeq[T]())

  implicit def sv_to_opt(sv:SingleValue)(implicit sim:Simulator):Option[AnyVal] = sv.update.value
  implicit def int_to_opt(int:Int):Option[AnyVal] = Some(int)
  implicit def float_to_opt(f:Float):Option[AnyVal] = Some(f)
  implicit def bool_to_opt(b:Boolean):Option[AnyVal] = Some(b)
  implicit def av_to_opt(a:AnyVal):Option[AnyVal] = Some(a)

  def convert(x:Any, op:Op)(implicit sim:Simulator):Any = (x,op) match {
    case (x:SingleValue, op:Op) => pirc.enums.convert(x.update.value, op)
    case (x, op) => pirc.enums.convert(x, op)
  }

  def eval(op:Op, ins:Any*)(implicit sim:Simulator):Option[AnyVal] = {
    eval(op, ins.map(in => convert(in, op))).asInstanceOf[Option[AnyVal]]
  }

  def isHigh(v:Option[AnyVal]):Option[Boolean] = v.map { 
    case v:Boolean => v
    case v => throw new Exception(s"Don't know how to check isHigh for $v")
  }
  def isLow(v:Option[AnyVal]):Option[Boolean] = v.map { 
    case v:Boolean => !v
    case v => throw new Exception(s"Don't know how to check isLow for $v")
  }
  //def IfElse[T](cond:Option[AnyVal])(trueFunc: => T)(falseFunc: => T)(implicit ev:TypeTag[T]):T = {
    //typeOf[T] match {
      //case t if t =:= typeOf[Unit] => 
        //isHigh(cond).foreach { 
          //case true => trueFunc
          //case false => falseFunc
        //}.asInstanceOf[T]
      //case t if t <:< typeOf[Option[_]] => 
        //isHigh(cond).flatMap { 
          //case true => trueFunc.asInstanceOf[Option[_]]
          //case false => falseFunc.asInstanceOf[Option[_]]
        //}.asInstanceOf[T]
    //}
  //}

  def Match(matches:(Any, () => Unit)*)(defaultFunc: => Unit)(implicit sim:Simulator):Unit = {
    var trigDefault = false
    val matchPairs:Seq[(Option[AnyVal], () => Unit)] = matches.map { 
      case (cond:SingleValue, func) => (cond.update.value, func)
      case (cond:Boolean, func) => (Some(cond), func)
      case (cond:Option[_], func) => (cond.asInstanceOf[Option[AnyVal]], func)
    }
    if (sim.inRegistration) {
      matchPairs.foreach { case (_, func) => func() }
      defaultFunc
    } else {
      matchPairs.foreach { case (cond, func) =>
        cond.foreach { 
          case true => 
            trigDefault = false
            func()
            return
          case false => trigDefault = true
        }
      }
      if (trigDefault) defaultFunc
    }
  }
  def IfElse(cond:Option[AnyVal])(trueFunc: => Unit)(falseFunc: => Unit)(implicit sim:Simulator) = {
    if (sim.inRegistration) { trueFunc; falseFunc }
    else {
      isHigh(cond).foreach { 
        case true => trueFunc
        case false => falseFunc
      }
    }
  }
  def If(cond:Option[AnyVal])(trueFunc: => Unit)(implicit sim:Simulator):Unit = {
    if (sim.inRegistration) { trueFunc }
    else {
      isHigh(cond).foreach {
        case true => trueFunc
        case false =>
      }
    }
  }
}
