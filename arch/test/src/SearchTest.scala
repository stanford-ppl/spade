package arch.test

import arch._

import spade.node._
import spade.util._
import spade.codegen._
import spade.traversal._
import spade.config._

import pirc.test._

import org.scalatest._
import sys.process._
import scala.language.postfixOps

class SearchTest extends UnitTest { self =>

  "SearchTest" should "success" taggedAs(ARCH) in {
    implicit val spade = SN4x4
    spade.top.config

    val router = new PlasticineGraphTraversal { implicit def arch = spade }

    val start = spade.cuArray(0)(0)
    val end = spade.cuArray(2)(2)

    val (path, cost) = router.simpleCostSearch(start, end, router.advance((n:Routable) => n.vouts, start) _)

    println(path, cost)
    val map = router.setConfig(SpadeMap.empty, path)

    new PlasticineCtrlDotPrinter().print(Some(map))
    s"out/bin/run -cp out/${spade}/CtrlNetwork".replace(".dot", "") !

    //new PlasticineScalarDotPrinter().print(Some(map))
    //s"out/bin/run -cp out/${arch}/ScalNetwork".replace(".dot", "") !

    //new PlasticineVectorDotPrinter().print(Some(map))
    //s"out/bin/run -cp out/${arch}/VecNetwork".replace(".dot", "") !

  }

}

