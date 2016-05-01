package com.cabrol.francois.mural.generator.rulebased.parameters

import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.{Chord, Scale}
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

/**
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   15-04-04
 */
class HarmonicProgressionTest extends FunSpec with BeforeAndAfter with Matchers {
  val harmDefC = HarmonicDefinition(new Chord("C"), new Scale("C major"))
  val harmDefA = HarmonicDefinition(new Chord("Am"), new Scale("A minor"))
  val chords: Map[Float, HarmonicDefinition] = Map((0, harmDefC), (1, harmDefA))
  val harmonicProgression = new HarmonicProgression(chords)

  describe("getHarmonyForTheTimePosition") {
    it("should return the good harmonic definition at time position 0") {
      val harmDef = harmonicProgression.getHarmonyForTheTimePosition(0)
      assert(harmDef == harmDefC)
    }
    it("should return the harmonic definition at time position 1") {
      val harmDef = harmonicProgression.getHarmonyForTheTimePosition(1)
      assert(harmDef == harmDefA)
    }
    it("should return the harmonic definition at time position 2") {
      val harmDef = harmonicProgression.getHarmonyForTheTimePosition(2)
      assert(harmDef == harmDefC)
    }
    it("should return the harmonic definition at time position 3") {
      val harmDef = harmonicProgression.getHarmonyForTheTimePosition(3)
      assert(harmDef == harmDefA)
    }
    it("should return the harmonic definition at time position 4") {
      val harmDef = harmonicProgression.getHarmonyForTheTimePosition(4)
      assert(harmDef == harmDefC)
    }
  }
}
