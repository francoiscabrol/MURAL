package com.cabrol.francois.mural.generator.rulebased.sequential.transition

import com.cabrol.francois.libjamu.musictheory.entity.note.{Note, RhythmicNote}
import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.{Chord, Scale, ScaleNote}
import com.cabrol.francois.mural.generator.rulebased.method.Methods
import com.cabrol.francois.mural.generator.rulebased.parameters._
import com.cabrol.francois.mural.generator.rulebased.sequential.MelodyCurveRandomizer
import com.cabrol.francois.mural.tools.Inspector
import com.cabrol.francois.mural.transition.PitchGenerator
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

/**
  * @author Francois Cabrol <francois.cabrol@live.fr>
  * @since 15-06-14
  */
class PitchGenerator$Test extends FunSpec with BeforeAndAfter with Matchers {

  val ambitus = new Ambitus(50, 71)
  val param = {
    val parentNotes = List()
    val hP = {
      val chords: Map[Float, HarmonicDefinition] = Map((0, HarmonicDefinition(new Chord("C"), new Scale("C major"))), (1, HarmonicDefinition(new Chord("D-"), new Scale("C major"))))
      new HarmonicProgression(chords)
    }
    val generationMethod = Methods.rulesBased
    val numBeatsPerBar = 4
    val numBars = 4
    val pSilence = 0
    val percentageOfNoteInChord = 50
    val numOfNoteAtTheSameTimeUnit = 1
    val density = 7
    val variance = 0
    val global = new GlobalParameters(generationMethod, parentNotes, numBeatsPerBar, numBars, ambitus, hP, pSilence, numOfNoteAtTheSameTimeUnit, Direction.both, variance, density, 0, percentageOfNoteInChord, PhraseParameters(Interval(0, 6), Interval(0, 0)))
    val dynamic: List[DynamicParameters] = List()
    Parameters(global, dynamic, 1)
  }

  val inspector = new Inspector(param)

  describe("keyFromScaleNote") {
    it("should return a note in the ambitus") {
      val keys = for (i <- 1 to 50) yield {
        PitchGenerator.keyFromScaleNote(new ScaleNote("C"), ambitus)
      }
      keys.foreach(key => {
        assert(inspector.inspectNoteHarmony(new Note(new RhythmicNote(1, 1), key)))
      })
    }
  }

  describe("randomizeKey") {
    it("should return a note in the ambitus") {
      val melodyCurbFactory = new MelodyCurveRandomizer()
      val keys = for (i <- 1 to 50) yield {
        PitchGenerator.randomizeKey(new RhythmicNote(1, 1), param, None, None, melodyCurbFactory)
      }
      keys.foreach(key => {
        assert(inspector.inspectNoteHarmony(new Note(new RhythmicNote(1, 1), key)))
      })
    }
  }

}
