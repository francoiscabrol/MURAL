package com.cabrol.francois.mural.generator.rulebased.sequential.transition

import com.cabrol.francois.mural.generator.rulebased.parameters.Density
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

/**
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   15-04-26
 */
class RhythmGenerator$Test extends FunSpec with BeforeAndAfter with Matchers {

  describe("The method"){
    describe("exponentialDistribution"){
      val duration = RhythmGenerator.convertDensityToDuration(6)
      assert(duration == Density.WHOLE_NOTE)
    }
  }
}