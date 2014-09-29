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

package com.cabrol.francois.mural.generator.rulebased.sequential.transition

import com.cabrol.francois.libjamu.musictheory.entity.note.{Key, RhythmicNote, Note}
import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.ScaleNote
import com.cabrol.francois.mural.generator.rulebased.sequential.MelodyCurveFactory
import com.cabrol.francois.mural.generator.rulebased.parameters.Parameters
import com.cabrol.francois.mural.transition.PitchGenerator


class TransitionalState(param:Parameters,
                        previousNote:Option[Note],
                        scaleNotePredefined:Option[ScaleNote],
                        rhythmicNotePredefined:Option[RhythmicNote],
                        keyNotePredefined:Option[Key],
                        melodyCurbFactory:MelodyCurveFactory) {

  private def getNewRhythmicNote =  rhythmicNotePredefined match {
    case None => RhythmGenerator.generateRhythmicNote(param, previousNote, None)
    case Some(r) => r.getDuration match {
      case d if d <= 0 => RhythmGenerator.generateRhythmicNote(param, previousNote, Some(r.getStart))
      case _ => r
    }
  }

  private def getNewKey(r:RhythmicNote) = keyNotePredefined match {
    case None => PitchGenerator.generateKey(r, param, scaleNotePredefined, previousNote, melodyCurbFactory)
    case Some(k) => k
  }

  def generateSingleNote:Note = {
    val r = getNewRhythmicNote
    val k = getNewKey(r)
    new Note(r, k)
  }

  def generateChord:List[Note] = {
    List()
  }

}
