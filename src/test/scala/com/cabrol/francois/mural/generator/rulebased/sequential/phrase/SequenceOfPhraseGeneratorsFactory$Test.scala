package com.cabrol.francois.mural.generator.rulebased.sequential.phrase

import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

/**
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   15-05-18
 */
class SequenceOfPhraseGeneratorsFactory$Test extends FunSpec with BeforeAndAfter with Matchers {

  describe("randomPhraseDuration") {
    it("should success and the duration should be > 0") {
      val duration = for (i <- 1 to 10) yield {
        SequenceOfPhraseGeneratorsFactory.randomPhraseDuration(10, 2)
      }
      duration.foreach(d => {
        assert(d >= 1)
      })
    }
  }
  
}
