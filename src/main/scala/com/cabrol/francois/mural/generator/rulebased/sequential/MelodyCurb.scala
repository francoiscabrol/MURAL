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

import com.cabrol.francois.mural.tools.RandomUtils
import com.cabrol.francois.mural.generator.rulebased.parameters.Direction

/**
 * Created with IntelliJ IDEA.
 * User: francois
 * Date: 2013-07-29
 * Time: 17:12
 * To change this template use File | Settings | File Templates.
 */

/*
case class MelodyCurb(val numBeats:Int, val samplingByBeat:Int){
  val curb:List[Int] = generateNewCurb

  lazy val samplingDynamicParametersInTotal = numBeats * samplingByBeat

  def generateNewCurb:List[Int] = {
    val probThatNextAddSameThanPrevious = RandomUtils.randomIntBetween(0, 100);
    val probThatNextPointUpperThanPrevious = RandomUtils.randomIntBetween(0, 100);
    val probThatNextPointSameThanPrevious = 10 //RandomUtils.randomIntBetween(0, 100);

    def newAdd(previous:Option[Int]):Int = {
      def randomPoint:Int = (RandomUtils.randomIntBetween(0,100, true)) match {
        case x if x > probThatNextPointUpperThanPrevious => 1
        case _ => -1
      }
      val add:Int = previous match {
        case None => randomPoint
        case Some(pre) => (RandomUtils.randomIntBetween(0,100, true)) match {
          case x if x > probThatNextAddSameThanPrevious => pre
          case _ => randomPoint
        }
      }
      add
    }
    def addPoint(curb:List[Int], previousPoint:Option[Int], previousAdd:Option[Int]):List[Int] = {
      def newPoint = {
        val add = newAdd(previousAdd)
        val p = curb.last + add
        addPoint((curb ::: List(p)), Some(p), Some(add))
      }

      if(curb.length >= samplingDynamicParametersInTotal)
        curb
      else{
        (RandomUtils.randomIntBetween(0,100)) match {
          case x if x > probThatNextPointSameThanPrevious => newPoint
          case _ => previousPoint match {
            case Some(p) => addPoint((curb ::: List(p)), Some(p), previousAdd)
            case _ => newPoint
          }
        }

      }
    }
    addPoint(List[Int](0), None, None)
  }

  def getDirectionPitchForNextNote(pos:Float):Direction.Direction = {
    Debug.curb(pos.toString)
    val idPoint:Int = (pos*samplingByBeat).toInt
    Debug.curb("id:" + idPoint)
    val size = curb.size
    val prePoint = curb(ListUtils.getIdOfAnInfiniteList(size, idPoint))
    val postPoint = curb(ListUtils.getIdOfAnInfiniteList(size, idPoint+1))
    val direction = (postPoint - prePoint) match {
      case x if x > 0 => Direction.up
      case x if x < 0 => Direction.down
      case _ => Direction.both
    }
    Debug.curb("direction : " + direction)
    direction

  }

}

*/


object MelodyCurbEnum extends Enumeration {
  type MelodyCurbEnum = Value
  val ascendant, descendant, brownien, random = Value

  def randomType:MelodyCurbEnum.MelodyCurbEnum = {
    val v = this.values
    v.toSeq(RandomUtils.randomIntBetween(0, this.maxId))
  }

}

class MelodyCurveFactory{

  lazy val probThatNextAddSameThanPrevious = RandomUtils.randomIntBetween(0, 100);
  lazy val probThatNextPointUpperThanPrevious = RandomUtils.randomIntBetween(0, 100);
  lazy val probThatNextPointSameThanPrevious = 10 //RandomUtils.randomIntBetween(0, 100);

  var curbType:MelodyCurbEnum.MelodyCurbEnum = MelodyCurbEnum.brownien

  def newDirection:Direction.Direction = {

    def upOrDown = {
      (RandomUtils.randomTrueOrFalse) match {
        case true => Direction.up
        case false => Direction.down
      }
    }

    curbType match {
      case MelodyCurbEnum.ascendant => Direction.up
      case MelodyCurbEnum.descendant => Direction.down
      case MelodyCurbEnum.brownien => upOrDown
      case MelodyCurbEnum.random =>
          (RandomUtils.randomIntBetween(0,100, true)) match {
            case x if x > probThatNextAddSameThanPrevious => Direction.both
            case _ => upOrDown
          }
    }
  }

  def randomizeCurbType = {
    curbType = MelodyCurbEnum.randomType
  }

}
