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

import com.cabrol.francois.mural.tools.{Debug, RandomUtils}
import com.cabrol.francois.mural.generator.rulebased.parameters.Direction

/**
 * Created with IntelliJ IDEA.
 * User: francois
 * Date: 2013-07-29
 * Time: 17:12
 * To change this template use File | Settings | File Templates.
 */

object MelodyCurbEnum extends Enumeration {
  type MelodyCurbEnum = Value
  val ascendant, descendant, brownien, random = Value

  def randomType:MelodyCurbEnum.MelodyCurbEnum = {
    val v = this.values
    val s = v.toSeq(RandomUtils.intBetween(0, this.maxId))
    Debug.log("melody_curve_type", s.toString)
    s
  }

}

class MelodyCurveRandomizer{

  def probThatNextAddSameThanPrevious = RandomUtils.intBetween(0, 100);

  var curbType:MelodyCurbEnum.MelodyCurbEnum = MelodyCurbEnum.randomType

  def newDirection:Direction.Direction = {

    def randomlyUpOrDown = {
      (RandomUtils.trueOrFalse) match {
        case true => Direction.up
        case false => Direction.down
      }
    }

    curbType match {
      case MelodyCurbEnum.ascendant => Direction.up
      case MelodyCurbEnum.descendant => Direction.down
      case MelodyCurbEnum.brownien => randomlyUpOrDown
      case MelodyCurbEnum.random =>
          (RandomUtils.intBetween(0,100)) match {
            case x if x > probThatNextAddSameThanPrevious => Direction.both
            case _ => randomlyUpOrDown
          }
    }
  }

  def randomizeCurbType = {
    curbType = MelodyCurbEnum.randomType
  }

}
