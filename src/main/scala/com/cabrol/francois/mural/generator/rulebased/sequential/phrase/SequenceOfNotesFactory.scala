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

import com.cabrol.francois.libjamu.musictheory.entity.note.Note
import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.ScaleNote
import com.cabrol.francois.mural.generator.rulebased.parameters.Parameters
import com.cabrol.francois.mural.tools.{Debug, RandomUtils}

/**
 * Create a sequence of note
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   2013-11-14
 */
object SequenceOfNotesFactory {

  private def generatePhrases(phraseGenerators:List[PhraseGenerator]):List[Note] = {
    // FIXME: Replace the recurive algorithm by using a map callback
    def generatePhrase(sequence:List[Note], phraseGenerators:List[PhraseGenerator], i:Int) : List[Note] = {
      if(i >= phraseGenerators.length)
        sequence
      else
        generatePhrase(sequence ::: phraseGenerators(i).generateThePhrase, phraseGenerators, i+1)
    }
    generatePhrase(List[Note](), phraseGenerators, 0)
  }

  /**
   * Generate a sequence of notes
   * @param parameters
   * @return the new random sequence of notes generated
   */
  def create(parameters : Parameters):List[Note] = {

    // Create the sequence of PhraseGenerator objects
    val phraseGenerators = sequenceOfPhraseGeneratorsFactory.create(parameters)

    // print output
    for(p <- phraseGenerators) Debug.sequenceGenerator(p.toString)
    Debug.sequenceGenerator("---- \n")

    // generate phrases from PhraseGenerator objects for building the new random sequence of notes and return it
    generatePhrases(phraseGenerators)
  }

}
