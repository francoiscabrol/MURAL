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
import com.cabrol.francois.mural.generator.rulebased.sequential.MelodyCurveFactory
import com.cabrol.francois.mural.tools.{Debug, RandomUtils}
import com.cabrol.francois.mural.generator.rulebased.parameters._
import com.cabrol.francois.mural.generator.rulebased.parameters.HarmonicDefinition
import scala.Some
import com.cabrol.francois.mural.generator.rulebased.parameters.Parameters

/**
 * Created with IntelliJ IDEA.
 * User: francois
 * Date: 2013-11-10
 * Time: 17:39
 * To change this template use File | Settings | File Templates.
 */

object GenerateOn extends Enumeration {
  type GenerateOn = Value
  val scale, chord = Value
}

object PitchGenerator {

  private def defineWhereGenerateOn(percentageNoteInChord:Int, forceNoteInChord:Boolean):GenerateOn.GenerateOn = {
    if ( forceNoteInChord == true || percentageNoteInChord > RandomUtils.randomIntBetween(0, 100) )
      GenerateOn.chord
    else
      GenerateOn.scale
  }

  private def ramdomOctaveInAmbitus(ambitus:Ambitus):Int = {
    val k:Int = RandomUtils.randomIntBetween(ambitus.min, ambitus.max)
    val key = new Key(k);
    key.getOctave();
  }
  private def generateNextKeyNotFromPrevious(ambitus:Ambitus, harmony:HarmonicDefinition, generateOn:GenerateOn.GenerateOn):Key = {
    Debug.pitchGenerator("The note depends on the previous one");
    val oct = ramdomOctaveInAmbitus(ambitus)
    val scaleNotesListUsed = generateOn match{
      case GenerateOn.chord => harmony.chord
      case GenerateOn.scale => harmony.scale
    }
    Debug.pitchGenerator("The " + generateOn + " contains " + scaleNotesListUsed)
    val sN:ScaleNote = scaleNotesListUsed.getScaleNote(RandomUtils.randomIntBetween(0, scaleNotesListUsed.getScaleNotes().size() - 1));
    sN.getKey(oct)
  }

  private def generateNewPitchFromScaleNoteDefined(scaleNotePredefined:ScaleNote, ambitus:Ambitus):Key = {
    Debug.pitchGenerator("The scale note is determined " + scaleNotePredefined)
    val oct = ramdomOctaveInAmbitus(ambitus)
    scaleNotePredefined.getKey(oct)
  }

  private def generateNextKeyFromPrevious(previousNote:Note, ambitus:Ambitus, direction: Direction.Direction, generateOn:GenerateOn.GenerateOn, harmony:HarmonicDefinition, harmonicProgression:HarmonicProgression):Key = {

    Debug.pitchGenerator("Is from previous " + previousNote)

    def previousKeyPositionFromRootInMode:Int = {
      val previousHarmony = harmonicProgression.getHarmonyForTheTimePosition(previousNote.getRhythmicNote.getStart)
      previousHarmony.modeRelativeToChord.getNotePosition(previousNote.getKey.getScaleNote)
    }

    def nextNoteKey(direction:Direction.Direction):Key = {

      val nextScaleNote:ScaleNote = {

        def newScaleNoteThatMatchWithChord(currentPos:Int, adding:Int):ScaleNote = {
          def tryNewPos(currentPos:Int):ScaleNote = {
            val newPos = currentPos + adding
            val scaleNoteInMode = harmony.modeRelativeToChord.getScaleNote(newPos)
            scaleNoteInMode match {
              case _ if harmony.chord.isIn(scaleNoteInMode) => scaleNoteInMode
              case _ => tryNewPos(newPos)
            }
          }
          tryNewPos(currentPos)
        }

        val addingPositionRelative = direction match {
          case Direction.up => 1
          case Direction.down => -1
          case Direction.both => 0
        }
        val newPositionInMode = previousKeyPositionFromRootInMode + addingPositionRelative
        val scaleNoteInMode = harmony.modeRelativeToChord.getScaleNote(newPositionInMode)
        Debug.pitchGenerator("Generate on " + generateOn + " containing " + harmony)
        generateOn match{
          case GenerateOn.scale => scaleNoteInMode
          case GenerateOn.chord =>  (harmony.chord.isIn(scaleNoteInMode)) match {
            case true => scaleNoteInMode
            case false => direction match {
              case Direction.up => newScaleNoteThatMatchWithChord(newPositionInMode, 1)
              case Direction.down => newScaleNoteThatMatchWithChord(newPositionInMode, -1)
              case Direction.both => newScaleNoteThatMatchWithChord(newPositionInMode, (if(RandomUtils.randomTrueOrFalse) -1 else 1))
            }
          }
        }
      }


      val nextKeyOctave:Int = {
        //place the note on the good octave
        val previousKeyOctave = previousNote.getKey.getOctave
        val keyOctave = {
          //si la note suivante est plus aigu que la note précédente sur la gamme
          if (previousNote.getKey.getScaleNote.getPitch < nextScaleNote.getPitch){
            direction match {
              case Direction.up => previousKeyOctave
              case Direction.down => previousKeyOctave - 1
              case Direction.both => previousKeyOctave
            }
          }
          else{
            direction match {
              case Direction.up => previousKeyOctave + 1
              case Direction.down => previousKeyOctave
              case Direction.both => previousKeyOctave
            }
          }
        }
        keyOctave
      }
      nextScaleNote.getKey(nextKeyOctave)
    }

    def nextKeyRespectConstraint:Key = {
      val k = nextNoteKey(direction)
      ambitus.contains(k.getMidiKey) match {
        case true => k
        case false => { Debug.pitchGenerator("The key " + k + " is not in the ambitus so it's going to be changed." ); generateNextKeyNotFromPrevious(ambitus, harmony, generateOn) }
        //            case false => nextNoteKey(direction match {
        //              case Direction.up => Direction.down
        //              case Direction.down => Direction.up
        //            })
      }
    }

    nextKeyRespectConstraint
  }

  def generateKey(rhythmicNote:RhythmicNote, param:Parameters, scaleNotePredefined:Option[ScaleNote], previousNote:Option[Note], melodyCurbFactory:MelodyCurveFactory, forceNoteInChord:Boolean = false):Key = {

    val dynamicParameters = param.getDynamic(rhythmicNote.getStart)
    //val currentBar = getCurrentBar(rhythmicNote.getStart)
    val harmony:HarmonicDefinition = param.global.harmonicProgression.getHarmonyForTheTimePosition(rhythmicNote.getStart)
    val generateOn:GenerateOn.GenerateOn = defineWhereGenerateOn(dynamicParameters.percentageNotesInChords, forceNoteInChord)
    val direction: Direction.Direction = melodyCurbFactory.newDirection

    Debug.pitchGenerator("Direction is " + direction + " and direction method is " + melodyCurbFactory.curbType)

    val key: Key = {
      scaleNotePredefined match {
        case None => previousNote match {
          case None => generateNextKeyNotFromPrevious(param.global.ambitus, harmony, generateOn)
          case Some(pN) => generateNextKeyFromPrevious(pN, param.global.ambitus, direction, generateOn, harmony, param.global.harmonicProgression)
        }
        case Some(sNP) => generateNewPitchFromScaleNoteDefined(sNP, param.global.ambitus)
      }

    }
    Debug.pitchGenerator("The new pitch is " + key + "\n")
    key
  }

}
