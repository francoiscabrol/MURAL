package com.cabrol.francois.mural.tools

import com.cabrol.francois.libjamu.musictheory.entity.note.Note
import com.cabrol.francois.mural.generator.rulebased.parameters.{HarmonicDefinition, Parameters}

/**
 * This tool allows to check if a notes sequence respect parameters
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   15-03-07
 */
class Inspector(params:Parameters) {

  case class InspectionException(message: String) extends Exception("[INSPECTION ERROR] " + message)
  
  def inspect(sequence:List[Note]):Boolean = {

    sequence.foreach(n => {
      inspectNoteHarmony(n)
      inspectRhythmicNote(n)
    })
    
    true
  }
  
  def inspectNoteHarmony(note:Note):Boolean = {
    // Test ambitus
    if (!params.global.ambitus.contains(note.getKey.getMidiKey())) {
      throw InspectionException(note + " is out of ambitus " + params.global.ambitus)
    }

    // Test percentage of notes in chord

    // Test notes are in the harmony progression or scale
    val harmony:HarmonicDefinition = params.global.harmonicProgression.getHarmonyForTheTimePosition(note.getRhythmicNote.getStart)
    if (!harmony.scale.isIn(note.getKey.getScaleNote))
      throw InspectionException(note.getKey.getScaleNote + " is not in the scale " + harmony.scale)

    true
  }
  
  def inspectRhythmicNote(note:Note):Boolean = {

    true
  }
  

}