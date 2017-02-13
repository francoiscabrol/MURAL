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

package com.cabrol.francois.mural.generator.rulebased.parameters

import com.cabrol.francois.libjamu.musictheory.entity.note.Note
import com.cabrol.francois.mural.generator.rulebased.method.GenerationMethod

object Direction extends Enumeration {
  type Direction = Value
  val up, down, both = Value
}

case class RangeInt(min: Int, max: Int) extends Range(min, max, 1)

case class PhraseParameters(duration: RangeInt = RangeInt(0, 5), gap: RangeInt = RangeInt(0, 3))

class Parameters(val method:GenerationMethod,
                 val parentNotes:List[Note],
                 val numBeatsPerBar:Int,
                 val numBars:Int,
                 val ambitus:Ambitus,
                 val harmonicProgression: HarmonicProgression,
                 val percentageOfSilence:Int,
                 val numOfNotesAtTheSameTimeUnit:Int,
                 val varianceDirection:Direction.Direction,
                 val variance:Int,
                 val rhythmicDensity: Density.DensityVal,
                 val variation:Int,
                 val percentageNotesInChords:Int,
                 val phrase:PhraseParameters = PhraseParameters()) {

  require(Range(0, 101).contains(percentageOfSilence), "The percentage of silence needs to be between 0 and 100")
  require(Range(0, 101).contains(percentageNotesInChords), "The percentage of notes in chords needs to be between 0 and 100")
  require(numBeatsPerBar >= 1, "Number of beats per bar in chords needs to be >= 1")
  require(numBars >= 1, "Number of bars in chords needs to be >= 1")
  require(numOfNotesAtTheSameTimeUnit >= 1, "Number of notes at the same time unit needs to be >= 1")

  def sequenceLenght:Int = numBars * numBeatsPerBar


}