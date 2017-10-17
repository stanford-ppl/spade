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
import pirc.util._

import org.scalatest._
import sys.process._
import scala.language.postfixOps

class SearchTest extends UnitTest { self =>

  "MinRoute" should "success" in {
    implicit val spade = SN8x8
    spade.top.config

    val logger = new Logger {
      override lazy val stream = newStream(s"${Config.outDir}/SearchTest", s"MinRoute.log")
    }

    val router = new PlasticineGraphTraversal { 
      implicit def arch = spade
      type M = SpadeMap
    }

    def testRouting(x1:Int, y1:Int, x2:Int, y2:Int) = {
      val start = spade.cuArray(x1)(y1)
      val end = spade.cuArray(x2)(y2)

      def validate(map:SpadeMap, cost:Int):SpadeMap = {
        val xMinCost = math.max(math.abs(x1-x2)-1, 0)
        val yMinCost = math.max(math.abs(y1-y2)-1, 0)

        assert(cost == xMinCost + yMinCost + 2)

        map
      }

      val map = router.simpleCostSearch(
        start=start, 
        end=end, 
        advance=router.advance((n:Routable) => n.couts, start) _,
        map=SpadeMap.empty,
        finPass=validate _,
        logger=None//Some(logger)
      )

      //new PlasticineCtrlDotPrinter(open=false).print(Some(map))

      //new PlasticineScalarDotPrinter().print(Some(map))

      //new PlasticineVectorDotPrinter(open=false).print(Some(map))
        

    }

    val numRow = 8
    val numCol = 8
    tic
    //val x1 = 0
    //val y1 = 5
    //val x2 = 0
    //val y2 = 0
    (0 until numCol).foreach { x1 =>
      (0 until numCol).foreach { x2 =>
        (0 until numRow).foreach { y1 =>
          (0 until numRow).foreach { y2 =>
            if ((x1 != x2) || y1 != y2) { testRouting(x1, y1, x2, y2) }
          }
        }
      }
    }
    toc(s"Route ${numRow * numRow * numCol * numCol} times", "s")
  }

  "SecondMinRoute" should "success" in {
    implicit val spade = SN8x8
    spade.top.config

    val logger = new Logger {
      override lazy val stream = newStream(s"${Config.outDir}/SearchTest", s"SecondMinRoute.log")
    }

    val router = new PlasticineGraphTraversal { 
      implicit def arch = spade
      type M = SpadeMap
    }

    def testRouting(x1:Int, y1:Int, x2:Int, y2:Int) = {
      val start = spade.cuArray(x1)(y1)
      val end = spade.cuArray(x2)(y2)

      var iter = 0
      var maxIter = 1

      def validate(map:SpadeMap, cost:Int):SpadeMap = {
        val xMinCost = math.max(math.abs(x1-x2)-1, 0)
        val yMinCost = math.max(math.abs(y1-y2)-1, 0)

        println(s"iter=$iter cost=$cost")
        if (iter != maxIter) {
          //new PlasticineCtrlDotPrinter(open=false).print(Some(map))
          iter += 1
          throw new Exception(s"Not valid route")
        } else {
          new PlasticineCtrlDotPrinter(open=true).print(Some(map))
        }
        map
      }

      router.simpleCostSearch(
        start=start, 
        end=end, 
        advance=router.advance((n:Routable) => n.couts, start) _,
        map=SpadeMap.empty,
        finPass=validate _,
        logger=Some(logger)
      )

    }

    val numRow = 8
    val numCol = 8
    tic
    val x1 = 0
    val y1 = 0
    val x2 = 2
    val y2 = 2
    if ((x1 != x2) || y1 != y2) { testRouting(x1, y1, x2, y2) }
    toc(s"Route ${numRow * numRow * numCol * numCol} times", "s")
  }

  "SpanTest" should "success" taggedAs(ARCH) in {
    implicit val spade = SN8x8
    spade.top.config

    val logger = new Logger {
      override lazy val stream = newStream(s"${Config.outDir}/SpanTest", s"SpanTest.log")
    }

    val router = new PlasticineGraphTraversal { 
      implicit def arch = spade
      type M = SpadeMap
    }

    def testSpanning(x1:Int, y1:Int, maxCost:Int) = {
      val start = spade.cuArray(x1)(y1)

      def advance(n:Routable, cost:Int) = {
        if (cost <= maxCost) {
          router.advance((n:Routable) => n.couts, start)(n,cost).map {
            case (n, a) => (n, 1)
          }
        } else {
          Nil
        }
      }

      val nodes = router.uniformCostSpan(
        start=start, 
        advance=advance _,
        logger=Some(logger)
      )
      
      var map = SpadeMap.empty
      nodes.foreach { case (n, c) =>
        map = map.setCF(n, DummyConfig())
      }
      new PlasticineCtrlDotPrinter(open=true).print(Some(map))

    }

    val numRow = 8
    val numCol = 8
    val maxCost = 4
    tic
    val x1 = 0
    val y1 = 0
    testSpanning(x1, y1, maxCost)
    toc(s"Span of maxCost=$maxCost", "s")
  }

}

