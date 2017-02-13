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

case class Interval(min:Float, max:Float)

case class PhraseParameters(duration:Interval = Interval(0, 5), gap:Interval = Interval(0, 3))

class GlobalParameters (val method:GenerationMethod,
                        val parentNotes:List[Note],
                        val numBeatsPerBar:Int,
                        val numBars:Int,
                        val ambitus:Ambitus,
                        val harmonicProgression: HarmonicProgression,
                        val percentageOfSilence:Int,
                        val numOfNotesAtTheSameTimeUnit:Int,
                        val varianceDirection:Direction.Direction,
                        val variance:Int,
                        override val rhythmicDensity: Density.DensityVal,
                        override val variation:Int,
                        override val percentageNotesInChords:Int,
                        val phrase:PhraseParameters = PhraseParameters()) extends DynamicParameters(rhythmicDensity,
                                                                                            variation,
                                                                                            percentageNotesInChords){

  require(Range(0, 101).contains(percentageOfSilence), "The percentage of silence needs to be between 0 and 100")
  require(Range(0, 101).contains(percentageNotesInChords), "The percentage of notes in chords needs to be between 0 and 100")
  require(numBeatsPerBar >= 1, "Number of beats per bar in chords needs to be >= 1")
  require(numBars >= 1, "Number of bars in chords needs to be >= 1")
  require(numOfNotesAtTheSameTimeUnit >= 1, "Number of notes at the same time unit needs to be >= 1")

  def sequenceLenght:Int = numBars * numBeatsPerBar

}

// TODO delete the dynamicParameters. they should exists only in genSession
class DynamicParameters(val rhythmicDensity:Density.DensityVal,
                        val variation:Int,
                        val percentageNotesInChords:Int)

case class Parameters(global:GlobalParameters, dynamic:List[DynamicParameters], samplingDynamicParametersPerBeat:Float){

  def getDynamic(positionInTime:Float):DynamicParameters = {
    dynamic match {
      case Nil => global
      case list:List[DynamicParameters] => dynamic.lift((positionInTime/samplingDynamicParametersPerBeat).ceil.toInt) match{
        case None => global
        case Some(a) => a
      }
    }
  }

  def samplingDynamicParametersInTotal:Int = math.floor(samplingDynamicParametersPerBeat * global.numBeatsPerBar).toInt


}
