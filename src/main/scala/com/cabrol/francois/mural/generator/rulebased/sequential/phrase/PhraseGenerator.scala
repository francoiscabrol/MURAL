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

package com.cabrol.francois.mural.generator.rulebased.sequential.phrase

import com.cabrol.francois.libjamu.musictheory.entity.note.{Note, RhythmicNote}
import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.ScaleNote
import com.cabrol.francois.mural.generator.rulebased.parameters.Parameters
import com.cabrol.francois.mural.generator.rulebased.sequential.MelodyCurveRandomizer
import com.cabrol.francois.mural.generator.rulebased.sequential.transition.TransitionalState
import com.cabrol.francois.mural.tools.Debug

/**
 *
 * @param startingPoint is the moment in the time where the phrase starts
 * @param endingPoint is the moment in the time where the phrase ends
 * @param startingNote is the function of the first note in the harmony
 * @param endingNote is the function of the end note in the harmony
 * @param param is the parameters used
 */
case class PhraseGenerator(val startingPoint:Float,
                           val endingPoint:Float,
                           val startingNote:ScaleNote,
                           val endingNote:ScaleNote,
                           val param:Parameters) {

  private val melodyCurveFactory:MelodyCurveRandomizer = new MelodyCurveRandomizer

  /**
   * Get the position in the time of the last note
   * @param notes is the sequence of notes of the current phrase
   * @return the last note
   */
  private def getLastNotePos(notes:List[Note]):Float = notes.last.getRhythmicNote.getStart + notes.last.getRhythmicNote.getDuration

  /**
   * Generate the first note
   * @return the first note
   */
  private def generateFirstNote:Note = {
    Debug.phraseGenerator("Generating the first note...")
    new TransitionalState(param, None, None, Some(new RhythmicNote(startingPoint, 0)), None, melodyCurveFactory).generateSingleNote
  }

  /**
   * Generate a new note (from the last one)
   * @param lastNote is the last note generated
   * @return the new note generated
   */
  private def generateNewNote(lastNote:Note):Note = {
    Debug.phraseGenerator("Generating another note...")
    new TransitionalState(param, Some(lastNote), None, None, None, melodyCurveFactory).generateSingleNote
  }

  /**
   * Generate the last note of the phrase
   * from the last one but with the pitch constrained by the param endingNote
   * @param lastNote is the last note generated
   * @return the new note generated
   */
  private def generateLastNote(lastNote:Note):Note = {
    Debug.phraseGenerator("Generating the last note...")
    new TransitionalState(param, Some(lastNote), Some(endingNote), None, None, melodyCurveFactory).generateSingleNote
  }

  /**
    * Generate the phrase
   * @return the generated phrase as a list of note
   */
  def generateThePhrase:List[Note] = {

    melodyCurveFactory.randomizeCurveType

    /**
     * Add a new note until
     * @param notes is the sequence of notes generated for the current phrase
     * @return the phrase with the new note generated as a list of notes
     */
    def addNewNote(notes:List[Note]):List[Note] = {
      if( getLastNotePos(notes) >= param.global.sequenceLenght - 1 || getLastNotePos(notes) >= endingPoint) {
        // if the last note in positioned after the end of the phrase or after the end of all the sequence
        // delete the last one and generate a new one with the good method to ending the phrase
        val n = notes.drop(-1)
        n ::: List(generateLastNote(n.last))
      }
      else
        addNewNote(notes ::: List(generateNewNote(notes.last)))
    }

    Debug.phraseGenerator("Generating a new phrase: " + "\n" + this.toString + "\n")
    addNewNote(List(generateFirstNote))
  }

  override def toString(): String = "Phrase: Start at " + startingPoint + " | End at " + endingPoint + " | First note is " + startingNote + " | Last note is " + endingNote
}
