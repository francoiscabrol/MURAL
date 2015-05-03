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
import com.cabrol.francois.mural.generator.rulebased.parameters.{Direction, Parameters}
import com.cabrol.francois.mural.tools.{Debug, RandomUtils}


object RhythmGenerator {

  def convertDensityToDuration(density:Int):Float = {
    //TODO unit tests!!
    val densityToDuration:List[Float] = List(0.25f, 0.5f, 0.75f, 1, 1.5f, 2, 4)
    val reverted = densityToDuration.reverse

    if(density < 0)
      return densityToDuration.last
    else if(density >= reverted.size)
      return densityToDuration.head

    reverted(density);
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
        val dynamicParameters = param.getDynamic(startingPoint)
        val density = dynamicParameters.rhythmicDensity
        val variance = param.global.variance
        val varianceDirection = param.global.varianceDirection

        val minDensity = varianceDirection match {
          case Direction.up => density
          case _ => density - variance
        }

        val maxDensity = varianceDirection match {
          case Direction.down => density
          case _ => density + variance
        }
        
        Debug.rhythmGenerator("minDensity:"+minDensity + " and maxDensity:"+maxDensity)
        
        RandomUtils.intBetween(minDensity, maxDensity)
      }

      Debug.rhythmGenerator("density choice:"+ densityChoice)
      convertDensityToDuration(densityChoice)
    }

    val startingPoint = newStartingPoint

    val duration = newDuration(startingPoint)
    
    new RhythmicNote(startingPoint, duration)
  }

}