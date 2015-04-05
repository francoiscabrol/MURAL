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
  type HarmonicScope = Value
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
   * @param forceNoteInChord
   * @return Non-determinist harmonic scope
   */
  private def harmonicScope(percentageNoteInChord:Int, forceNoteInChord:Boolean):HarmonicScope.HarmonicScope = {
    val r = RandomUtils.intBetween(0, 100)
    if ( forceNoteInChord == true || percentageNoteInChord > r )
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
    val key = new Key(k);
    key.getOctave();
  }

  /**
   * Choose a new octave
   * @param nextScaleNote
   * @return Non-deterministic octave number
   */
  private def octave(nextScaleNote: ScaleNote, previousNote:Note, direction: Direction.Direction):Int = {
    //place the note on the good octave
    val previousKeyOctave = previousNote.getKey.getOctave
    val keyOctave = {
      //si la note suivante est plus aigu que la note précédente sur la gamme
      if (previousNote.getKey.getScaleNote.getPitch < nextScaleNote.getPitch){
        direction match {
          case Direction.up   => previousKeyOctave
          case Direction.down => previousKeyOctave - 1
          case Direction.both => previousKeyOctave
        }
      }
      else{
        direction match {
          case Direction.up   => previousKeyOctave + 1
          case Direction.down => previousKeyOctave
          case Direction.both => previousKeyOctave
        }
      }
    }
    keyOctave
  }

  /**
   * Choose a new scale note from current position
   * @param currentPos
   * @param adding
   * @param harmony
   * @return Non-deterministic scale note
   */
  private def randomizeScaleNote(currentPos:Int, adding:Int, harmony:HarmonicDefinition):ScaleNote = {
    def randomScaleNote(currentPos:Int):ScaleNote = {
      val newPos = currentPos + adding
      val scaleNoteInMode = harmony.modeRelativeToChord.getScaleNote(newPos)
      scaleNoteInMode match {
        case _ if harmony.chord.isIn(scaleNoteInMode) => scaleNoteInMode
        case _ => randomScaleNote(newPos)
      }
    }
    randomScaleNote(currentPos)
  }

  /**
   * Choose a new scale note
   * @param previousNote
   * @param harmony
   * @param harmonicScope
   * @param harmonicProgression
   * @param direction
   * @return Non-deterministic scale note
   */
  private def randomizeScaleNote(previousNote:Note, harmony:HarmonicDefinition, harmonicScope: HarmonicScope.HarmonicScope, harmonicProgression: HarmonicProgression, direction: Direction.Direction):ScaleNote = {

    val relativePosition = direction match {
      case Direction.up   => 1
      case Direction.down => -1
      case Direction.both => 0
    }
    val newPositionInMode = extractPreviousKeyPositionFromRootInMode(harmony, harmonicProgression, previousNote) + relativePosition
    val scaleNoteInMode = harmony.modeRelativeToChord.getScaleNote(newPositionInMode)
    Debug.pitchGenerator("Generate on " + harmonicScope + " containing " + harmony)
    harmonicScope match{
      case HarmonicScope.scale => scaleNoteInMode
      case HarmonicScope.chord =>  (harmony.chord.isIn(scaleNoteInMode)) match {
        case true => scaleNoteInMode
        case false => direction match {
          case Direction.up   => randomizeScaleNote(newPositionInMode, 1, harmony)
          case Direction.down => randomizeScaleNote(newPositionInMode, -1, harmony)
          case Direction.both => randomizeScaleNote(newPositionInMode, (if(RandomUtils.trueOrFalse) -1 else 1), harmony)
        }
      }
    }
  }

  /**
   * Choose a new key from constraints (the ambitus, the harmony definition and the harmonic scope)
   * @param ambitus
   * @param harmony The definition of the harmony (the chord, the scale and the relative mode)
   * @param harmonicScope The harmonic scope (could be the scale or the chord)
   * @return Non-deterministic key
   */
  private def keyFromConstraints(ambitus:Ambitus, harmony:HarmonicDefinition, harmonicScope:HarmonicScope.HarmonicScope):Key = {
    Debug.pitchGenerator("The next note depends on contraints only")
    //val oct = octave(ambitus)
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
  private def keyFromScaleNote(scaleNotePredefined:ScaleNote, ambitus:Ambitus):Key = {
    Debug.pitchGenerator("The scale note is determined :" + scaleNotePredefined)
    val oct = octave(ambitus)
    scaleNotePredefined.getKey(oct)
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
  private def keyFromPreviousOne(previousNote:Note, ambitus:Ambitus, melodyCurbFactory: MelodyCurveRandomizer, harmonicScope:HarmonicScope.HarmonicScope, harmony:HarmonicDefinition, harmonicProgression:HarmonicProgression):Key = {

    Debug.pitchGenerator("The next note is comming from previous " + previousNote)

    val direction: Direction.Direction = melodyCurbFactory.newDirection

    Debug.pitchGenerator("Direction is " + direction + " and direction method is " + melodyCurbFactory.curbType)

    val scaleNote = randomizeScaleNote(previousNote, harmony, harmonicScope, harmonicProgression, direction)
    scaleNote.getKey(octave(scaleNote, previousNote, direction))
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
   * @param forceNoteInChord
   * @return Non-deterministic key
   */
  def randomizeKey(rhythmicNote:RhythmicNote, param:Parameters, scaleNotePredefined:Option[ScaleNote], previousNote:Option[Note], melodyCurbFactory:MelodyCurveRandomizer, forceNoteInChord:Boolean = false):Key = {

    val dynamicParameters = param.getDynamic(rhythmicNote.getStart)
    val harmony:HarmonicDefinition = param.global.harmonicProgression.getHarmonyForTheTimePosition(rhythmicNote.getStart)
    val scope:HarmonicScope.HarmonicScope = harmonicScope(dynamicParameters.percentageNotesInChords, forceNoteInChord)

    val key: Key = {
      scaleNotePredefined match {
        case None => {
          previousNote match {
            case None            => keyFromConstraints(param.global.ambitus, harmony, scope)
            case Some(scaleNote) => keyFromPreviousOne(scaleNote, param.global.ambitus, melodyCurbFactory, scope, harmony, param.global.harmonicProgression)
          }
        }
        case Some(sNP) => keyFromScaleNote(sNP, param.global.ambitus)
      }

    }
    Debug.pitchGenerator("The new pitch is " + key + "\n")
    key
  }

}
