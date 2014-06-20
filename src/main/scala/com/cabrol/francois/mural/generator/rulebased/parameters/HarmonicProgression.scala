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

import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.{Scale, ScaleNote, Mode, Chord}

case class HarmonicDefinition(chord:Chord, scale:Scale){
  val _modeRelativeToChord:Mode = {
    val rootChord:ScaleNote = chord.getRoot
    val rootChordPositionInScale = scale.getNotePosition(rootChord)
    scale.getMode(rootChordPositionInScale)
  }

  def modeRelativeToChord:Mode = _modeRelativeToChord
}

class HarmonicProgression(val chords:Map[Float, HarmonicDefinition]) {

    def getHarmonyForTheTimePosition(pos:Float):HarmonicDefinition = {
      val d = chords.filter(k => k._1 <= pos)
      d.last._2
    }
}
