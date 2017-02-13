/*
 * Copyright (c) 2014 François Cabrol.
 *
 *  This file is part of MURAL.
 *
 *     MURAL is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     MURAL is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with MURAL.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.cabrol.francois.mural.transition

import com.cabrol.francois.libjamu.musictheory.entity.note.{RhythmicNote, Note, Key}
import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.ScaleNote
import com.cabrol.francois.mural.generator.rulebased.sequential.MelodyCurveRandomizer
import com.cabrol.francois.mural.tools.{Debug, RandomUtils}
import com.cabrol.francois.mural.generator.rulebased.parameters._
import com.cabrol.francois.mural.generator.rulebased.parameters.HarmonicDefinition
import com.cabrol.francois.mural.generator.rulebased.parameters.Parameters


/**
 * Enum that define the scope of the harmony (scale or chord)
 */
object HarmonicScope extends Enumeration {
  val scale, chord = Value
}

/**
 * Constains every methods needed to choose a new key (ScaleNote + octave)
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   2013-11-10
 */
object PitchGenerator {

  /**
   * Choose an harmonic scope definition between both sets, chord's degrees and scale's degrees
   * @param percentageNoteInChord
   * @return Non-determinist harmonic scope
   */
  private def harmonicScope(percentageNoteInChord:Int):HarmonicScope.Value = {
    val r = RandomUtils.intBetween(0, 100)
    if (percentageNoteInChord > r )
      HarmonicScope.chord
    else
      HarmonicScope.scale
  }

  /**
   * Choose octave in the ambitus
   * @param ambitus
   * @return Non-deterministic octave number
   */
  private def octave(ambitus:Ambitus):Int = {
    val k:Int = RandomUtils.intBetween(ambitus.min, ambitus.max)
    val key   = new Key(k);
    key.getOctave();
  }

  /**
   * Choose a new key from constraints (the ambitus, the harmony definition and the harmonic scope)
   * @param ambitus
   * @param harmony The definition of the harmony (the chord, the scale and the relative mode)
   * @param harmonicScope The harmonic scope (could be the scale or the chord)
   * @return Non-deterministic key
   */
  private def keyFromConstraints(ambitus:Ambitus, harmony:HarmonicDefinition, harmonicScope:HarmonicScope.Value):Key = {
    Debug.pitchGenerator("The next note depends on contraints only")
    val scaleNotesAvailable = harmonicScope match {
      case HarmonicScope.chord => harmony.chord
      case HarmonicScope.scale => harmony.scale
    }
    Debug.pitchGenerator("The " + harmonicScope + " contains " + scaleNotesAvailable)

    // List all pitches available
    val pitchesAvailable = ambitus.filter(pitch => scaleNotesAvailable.isIn(new ScaleNote(pitch)))
    
    // Return a random key
    val index = RandomUtils.intBetween(0, pitchesAvailable.size - 1)
    new Key(pitchesAvailable.apply(index))
  }

  /**
   * Choose a new key from the scale note
   * @param scaleNotePredefined
   * @param ambitus
   * @return Non-deterministic key
   */
  def keyFromScaleNote(scaleNotePredefined:ScaleNote, ambitus:Ambitus):Key = {
    Debug.pitchGenerator("The scale note is determined :" + scaleNotePredefined)
    val pitchesAvailable = ambitus.filter(key => scaleNotePredefined.getPitch() == new ScaleNote(key).getPitch())
    new Key(RandomUtils.randomElement(pitchesAvailable))
  }

  /**
   * Choose a new key from the previous one
   * @param previousNote
   * @param ambitus
   * @param harmonicScope
   * @param harmony
   * @param harmonicProgression
   * @return Non-deterministic key
   */
  private def keyFromPreviousOne(previousNote:Note, ambitus:Ambitus, melodyCurveFactory: MelodyCurveRandomizer, harmonicScope:HarmonicScope.Value, harmony:HarmonicDefinition, harmonicProgression:HarmonicProgression):Key = {

    Debug.pitchGenerator("[keyFromPreviousOne] The next note is comming from previous " + previousNote)

    val direction: Direction.Direction = melodyCurveFactory.newDirection

    Debug.pitchGenerator("[keyFromPreviousOne] Direction is " + direction + " and curve type is " + melodyCurveFactory.curveType)
    
    val scaleNotesAvailable = harmonicScope match {
      case HarmonicScope.chord => harmony.chord
      case HarmonicScope.scale => harmony.scale
    }
    Debug.pitchGenerator("The " + harmonicScope + " contains " + scaleNotesAvailable)

    // List all pitches available
    val pitchesAvailable = ambitus.filter(pitch => scaleNotesAvailable.isIn(new ScaleNote(pitch)))
    
    // Find previous note function
    val previousPositionInMode = extractPreviousKeyPositionFromRootInMode(harmony, harmonicProgression, previousNote)
    val scaleNoteInMode = harmony.modeRelativeToChord.getScaleNote(previousPositionInMode)
    val scaleNoteInHamony =  {
      def find(positive:Boolean, i:Int): ScaleNote = {
        val testPitch = positive match {
          case true  => scaleNoteInMode.getPitch + i
          case false => scaleNoteInMode.getPitch - i
        }
        if (scaleNotesAvailable.isIn(new ScaleNote(testPitch))) {
          return new ScaleNote(testPitch)
        }

        if (positive)
          find(false, i)
        else
          find(true, i+1)
      }
      find(true, 0)
    }

    // Find previous note position
    val previousNotePosition = {
      def find(positive:Boolean, i:Int): Int = {
        val testPitch = positive match {
          case true  => previousNote.getKey.getMidiKey + i
          case false => previousNote.getKey.getMidiKey - i
        }
        // TODO write unit test for this anonymous function
        val previousNotePosition = pitchesAvailable.indexOf(testPitch)
        if (new ScaleNote(testPitch).toString == scaleNoteInHamony.toString && previousNotePosition > -1) {
          return previousNotePosition
        }
          
        if (positive)
          find(false, i)
        else
          find(true, i+1)
      }
      find(true, 0)
    }

    val relativePosition = direction match {
      case Direction.up   => 1
      case Direction.down => -1
      case Direction.both => 0
    }

    //val p = Math.floorMod((previousNotePosition + relativePosition), pitchesAvailable.size)
    val position:Int = {
      val p:Int = previousNotePosition + relativePosition
      if (p >= 0 && p < pitchesAvailable.size) {
        p
      }
      else {
        Debug.pitchGenerator("Since the note would be out of ambitus, we change the randomize curve")
        melodyCurveFactory.randomizeCurveType
        previousNotePosition - relativePosition
      }
    } ensuring(_ >= 0, "new position should be > 0")
                                           
    new Key(pitchesAvailable.apply(position))
  }

  private def extractPreviousKeyPositionFromRootInMode(harmonicDefinition: HarmonicDefinition, harmonicProgression: HarmonicProgression, previousNote:Note):Int = {
    val previousHarmony = harmonicProgression.getHarmonyForTheTimePosition(previousNote.getRhythmicNote.getStart)
    previousHarmony.modeRelativeToChord.getNotePosition(previousNote.getKey.getScaleNote)
  }

  /**
   *
   * @param rhythmicNote
   * @param param
   * @param scaleNotePredefined
   * @param previousNote
   * @param melodyCurbFactory
   * @return Non-deterministic key
   */
  def randomizeKey(rhythmicNote:RhythmicNote, param:Parameters, scaleNotePredefined:Option[ScaleNote], previousNote:Option[Note], melodyCurbFactory:MelodyCurveRandomizer):Key = {

    val harmony:HarmonicDefinition        = param.harmonicProgression.getHarmonyForTheTimePosition(rhythmicNote.getStart)
    val scope:HarmonicScope.Value = harmonicScope(param.percentageNotesInChords)

    val key: Key = {
      scaleNotePredefined match {
        case None => {
          previousNote match {
            case None            => keyFromConstraints(param.ambitus, harmony, scope)
            case Some(scaleNote) => keyFromPreviousOne(scaleNote, param.ambitus, melodyCurbFactory, scope, harmony, param.harmonicProgression)
          }
        }
        case Some(sNP) => keyFromScaleNote(sNP, param.ambitus)
      }

    }
    Debug.pitchGenerator("The new pitch is " + key + "\n")
    key
  }

}
