package com.cabrol.francois.mural.tools

import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

/**
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   15-04-19
 */
class RandomUtils$Test extends FunSpec with BeforeAndAfter with Matchers {

  describe("The method"){
    describe("exponentialDistribution"){
      it("should return random number in the range") {
        val min = 0
        val max = 1
        val results = for (i <- 1 to 10) yield {
          RandomUtils.exponentialDistribution()
        }
        results.foreach(number => {
          assert(number >= min)
          assert(number <= max)
        })
      }
    }
    describe("exponentialDistributionBetween"){
      it("should return random number in the range") {
        val min = 0
        val max = 100
        val results = for (i <- 1 to 10) yield {
          RandomUtils.exponentialDistributionBetween(min, max)
        }
        results.foreach(number => {
          assert(number >= min)
          assert(number <= max)
        })
      }
    }
  }

}
