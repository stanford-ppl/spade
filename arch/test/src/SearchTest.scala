package arch.test

import arch._

import spade.node._
import spade.util._
import spade.codegen._
import spade.traversal._
import spade.config._

import pirc._
import pirc.test._
import pirc.codegen._

import org.scalatest._
import sys.process._
import scala.language.postfixOps

class SearchTest extends UnitTest { self =>

  "SearchTest" should "success" taggedAs(ARCH) in {
    implicit val spade = SN4x4
    spade.top.config

    val logger = new Logger {
      override lazy val stream = newStream(s"${Config.outDir}/SearchTest", s"SearchTest.log")
    }

    val router = new PlasticineGraphTraversal { implicit def arch = spade }

    val start = spade.cuArray(0)(0)
    val end = spade.cuArray(2)(2)

    val (path, cost) = router.simpleCostSearch(
      start=start, 
      end=end, 
      advance=router.advance((n:Routable) => n.vouts, start) _,
      logger=Some(logger)
    )

    val map = router.setConfig(SpadeMap.empty, path)

    //new PlasticineCtrlDotPrinter().print(Some(map))

    //new PlasticineScalarDotPrinter().print(Some(map))

    new PlasticineVectorDotPrinter(open=true).print(Some(map))

  }

}

