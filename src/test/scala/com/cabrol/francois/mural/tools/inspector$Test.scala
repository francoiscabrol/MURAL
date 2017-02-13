package com.cabrol.francois.mural.tools

import com.cabrol.francois.libjamu.musictheory.entity.note.{Key, RhythmicNote, Note}
import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.{Chord, Scale}
import com.cabrol.francois.mural.generator.rulebased.method.Methods
import com.cabrol.francois.mural.generator.rulebased.parameters._
import org.scalatest.{Matchers, BeforeAndAfter, FunSpec}

class Inspector$Test extends FunSpec with BeforeAndAfter with Matchers {

  val parameters = {
    val parentNotes = List()
    val chords: Map[Float, HarmonicDefinition] = Map((0, HarmonicDefinition(new Chord("C"), new Scale("C major"))), (1, HarmonicDefinition(new Chord("Am"), new Scale("A minor"))))
    val hP = new HarmonicProgression(chords)
    val generationMethod = Methods.RULESBASED
    val numBeatsPerBar = 4
    val numBars = 4
    val ambitus: Ambitus = new Ambitus(28, 71)
    val pSilence = 0
    val percentageOfNoteInChord = 50
    val numOfNoteAtTheSameTimeUnit = 1
    val density = Density.EIGHT_NOTE
    val variance = 0
    new Parameters(generationMethod, parentNotes, numBeatsPerBar, numBars, ambitus, hP, pSilence, numOfNoteAtTheSameTimeUnit, Direction.up, variance, density, 0, percentageOfNoteInChord)
  }
  
  val inspector = new Inspector(parameters)
  
  describe("inspectNoteHarmony") {
    it("should failed if the note is out the ambitus") {
      val note = new Note(new RhythmicNote(0, 1), new Key(72))
      a [Exception] should be thrownBy {
        inspector.inspectNoteHarmony(note)
      }
      //error.get should include regex "ambitus"
    }
    it("should succeed if the note is in the ambitus") {
      val note = new Note(new RhythmicNote(0, 1), new Key(60))
      val success = inspector.inspectNoteHarmony(note)
      assert(success)
    }
    it ("should succeed if the note is in the harmony") {
      val note = new Note(new RhythmicNote(0, 1), new Key(64)) //E
      val success = inspector.inspectNoteHarmony(note)
      assert(success)
    }
    it ("should failed if the note is not in the harmony") {
      val note = new Note(new RhythmicNote(0, 1), new Key(66)) //E
      a [Exception] should be thrownBy {
        inspector.inspectNoteHarmony(note)
      }
      //error.get should include regex "not in the scale"
    }
  }
  
  describe("inspectRhythmicNote") {
    it ("should failed if the note is out of time boundaries") {
      val note = new Note(new RhythmicNote(65, 2), new Key(60))
      the [Exception] thrownBy {
        inspector.inspectRhythmicNote(note)
      } getMessage() should include ("out of sequence")
    }
    it ("should success if the note is contains between the time boundaries") {
      val note = new Note(new RhythmicNote(0, 65), new Key(60))
      noException should be thrownBy inspector.inspectRhythmicNote(note)
    }
  }
}