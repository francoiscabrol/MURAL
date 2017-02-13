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

import com.cabrol.francois.libjamu.musictheory.entity.note.{Note, RhythmicNote}
import com.cabrol.francois.mural.generator.rulebased.parameters.{Density, Direction, Parameters}
import com.cabrol.francois.mural.tools.{Debug, RandomUtils}


object RhythmGenerator {

  val densities = List(Density.SIXTEENTH_NOTE, Density.EIGHT_NOTE, Density.QUARTER_NOTE, Density.HALF_NOTE, Density.WHOLE_NOTE)

  def convertDensityToDuration(densityIndex:Int):Density.DensityVal = {
    //TODO unit tests!!
    if(densityIndex < 0)
      return densities.head
    else if(densityIndex >= densities.size)
      return densities.last

    densities(densityIndex)
  }
  
  def generateRhythmicNote(param:Parameters, previousNote:Option[Note], startingPointDefined:Option[Float]):RhythmicNote = {

    def newStartingPoint:Float = startingPointDefined match {
        case Some(s) => s
        case None => previousNote match {
          case Some(n) => (n.getRhythmicNote.getStart + n.getRhythmicNote.getDuration)
          case None => 0
        }
    }

    def newDuration(startingPoint:Float):Float = {

      val densityChoice:Int = {
        val density = param.rhythmicDensity
        val variance = param.variance
        val varianceDirection = param.varianceDirection

        val minDensity = varianceDirection match {
          case Direction.up => densities.indexOf(density)
          case _ => densities.indexOf(density) - variance
        }

        val maxDensity = varianceDirection match {
          case Direction.down => densities.indexOf(density)
          case _ => densities.indexOf(density) + variance
        }
        
        Debug.rhythmGenerator("minDensity:"+minDensity + " and maxDensity:"+maxDensity)
        
        RandomUtils.intBetween(minDensity, maxDensity)
      }

      Debug.rhythmGenerator("density choice:"+ densityChoice)
      val density = convertDensityToDuration(densityChoice)
      density.getBeatPercentage
    }

    val startingPoint = newStartingPoint

    val duration = newDuration(startingPoint)
    
    new RhythmicNote(startingPoint, duration)
  }

}