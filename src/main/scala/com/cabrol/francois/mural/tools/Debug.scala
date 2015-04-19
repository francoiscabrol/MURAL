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

import scala.io.Codec
import scala.reflect.io.File

/**
 * Created with IntelliJ IDEA.
 * User: francois
 * Date: 2013-08-17
 * Time: 12:34
 * To change this template use File | Settings | File Templates.
 */
object Debug {

  val transitionalState =false
  val phraseGenerator   =true
  val sequenceGenerator =false
  val pitchGenerator    =false
  val curve             =false
  val streamGenerator   =false

  def streamGenerator(msg:String):Unit   = if(streamGenerator)   println("[STREAM_GENERATOR] "   + msg)
  
  def transitionalState(msg:String):Unit = if(transitionalState) println("[TRANSITIONAL_STATE] " + msg)

  def sequenceGenerator(msg:String):Unit = if(sequenceGenerator) println("[SEQUENCE_GENERATOR] " + msg)

  def pitchGenerator(msg:String):Unit    = if(pitchGenerator)    println("[PITCH_GENERATOR] "    + msg)

  def phraseGenerator(msg:String):Unit   = if(phraseGenerator)   println("[PHRASE_GENERATOR] "   + msg)

  def curve(msg:String):Unit             = if(curve)             println("[CURB] "               + msg)
  
  def log(filename:String, log:AnyVal) = {
    val output = File(filename + ".txt").writer(true, Codec.UTF8)
    output.append(log + " ")
    output.close
  }

  private def setColor(s:String):String  = s match {
    case "blue" => Console.BLUE
  }

}
