package com.cabrol.francois.mural.generator.rulebased.sequential.transition

import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.ScaleNote
import com.cabrol.francois.mural.generator.rulebased.parameters.Ambitus
import com.cabrol.francois.mural.transition.PitchGenerator
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

/**
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   15-06-14
 */
class PitchGenerator$Test extends FunSpec with BeforeAndAfter with Matchers {

describe("keyFromScaleNote") {
  it("should return a note in the ambitus") {
    val ambitus = new Ambitus(50, 71)
    val keys = for (i <- 1 to 50) yield {
      PitchGenerator.keyFromScaleNote(new ScaleNote("C"), ambitus)
    }
    keys.foreach(key => {
      assert(ambitus.contains(key.getMidiKey()))
    })
  }
}

}
