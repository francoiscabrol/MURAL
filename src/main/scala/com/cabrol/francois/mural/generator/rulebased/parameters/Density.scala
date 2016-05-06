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


object Density extends Enumeration {

  val WHOLE_NOTE  = DensityVal("Whole note", 1/4)
  val HALF_NOTE   = DensityVal("Half note", 1/2)
  val QUARTER_NOTE = DensityVal("Quarter note", 1)
  val EIGHT_NOTE = DensityVal("Eight note", 2)
  val SIXTEENTH_NOTE = DensityVal("Sixteenth note", 4)

  case class DensityVal(name: String, numberPerBeat: Float) extends super.Val(name) {
    def getDotted:Float = (getBeatPercentage * 1.5).toFloat
    //TODO def getTernary:Float = ?
    def getBeatPercentage:Float = 1 / numberPerBeat
  }

  implicit def convert(value: Value) = value.asInstanceOf[DensityVal]
}
