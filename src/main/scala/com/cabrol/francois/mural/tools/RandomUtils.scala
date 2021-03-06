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

package com.cabrol.francois.mural.tools

import scala.util.Random

/**
 * Created with IntelliJ IDEA.
 * User: francois
 * Date: 2013-07-29
 * Time: 18:16
 */
object RandomUtils {
  def randomElement[B](list: Seq[B]):B = {
    val i = Random.nextInt(list.length)
    list(i)
  }

  def intBetween(min: Int, max: Int): Int = {
    min + Random.nextInt(max - min)
  }

  def floatBetween(min:Float, max:Float):Float = {
    val length = max - min
    min + Random.nextFloat() * length
  }
  
  def exponentialDistribution(rate:Double=0.8):Double = {
    val u = Random.nextFloat()
    math.exp(-4 * u)
  }

  def exponentialDistributionBetween(min:Float, max:Float, rate:Double=0.8):Double = {
    val rand = exponentialDistribution(rate)
    val length = max - min
    min + rand * length
  }

  def trueOrFalse:Boolean = {
    Random.nextBoolean()
  }

}
