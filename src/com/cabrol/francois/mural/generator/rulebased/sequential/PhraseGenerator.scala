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

package com.cabrol.francois.mural.generator.rulebased.sequential

import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.ScaleNote
import com.cabrol.francois.libjamu.musictheory.entity.note.{RhythmicNote, Note}
import com.cabrol.francois.mural.generator.rulebased.parameters.Parameters
import com.cabrol.francois.mural.tools.Debug
import com.cabrol.francois.mural.generator.rulebased.transition.TransitionalState


case class PhraseGenerator(val startingPoint:Float,
                           val endingPoint:Float,
                           val startingNote:ScaleNote,
                           val endingNote:ScaleNote,
                           val param:Parameters) {

  def generateThePhrase:List[Note] = {

    val melodyCurveFactory:MelodyCurveFactory = new MelodyCurveFactory
    melodyCurveFactory.randomizeCurbType

    def getLastNotePos(notes:List[Note]):Float = notes.last.getRhythmicNote.getStart + notes.last.getRhythmicNote.getDuration

    def generateFirstNote:Note = {
      Debug.phraseGenerator("Generating the first note...")
      new TransitionalState(param, None, Some(startingNote), Some(new RhythmicNote(startingPoint, 0)), None, melodyCurveFactory).generateSingleNote
    }

    def generateNewNote(lastNote:Note):Note = {
      Debug.phraseGenerator("Generating another note...")
      new TransitionalState(param, Some(lastNote), None, None, None, melodyCurveFactory).generateSingleNote
    }

    def addNewNote(notes:List[Note]):List[Note] = {
      if( getLastNotePos(notes) >= param.global.sequenceLenght || getLastNotePos(notes) >= endingPoint){
        notes.drop(-1) ::: List(generateLastNote(notes.last))
      }
      else
        addNewNote(notes ::: List(generateNewNote(notes.last)))
    }

    def generateLastNote(lastNote:Note):Note = {
      Debug.phraseGenerator("Generating the last note...")
      new TransitionalState(param, Some(lastNote), Some(endingNote), None, None, melodyCurveFactory).generateSingleNote
    }

    Debug.phraseGenerator("Generating a new phrase: " + "\n" + this.toString + "\n")
    addNewNote(List(generateFirstNote))
  }

  override def toString(): String = "Phrase: Start at " + startingPoint + " | End at " + endingPoint + " | First note is " + startingNote + " | Last note is " + endingNote
}
