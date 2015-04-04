package com.cabrol.francois.mural.tools

import com.cabrol.francois.libjamu.musictheory.entity.note.Note
import com.cabrol.francois.mural.generator.rulebased.parameters.{HarmonicDefinition, Parameters}

/**
 * This tool allows to check if a notes sequence respect parameters
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   15-03-07
 */
class Inspector(params:Parameters) {

  def inspect(sequence:List[Note]):(Boolean, Option[String]) = {

    sequence.foreach(n => {
      
      val (success, error) = inspectNoteHarmony(n)
      if (!success)
        failed(error.get)
      
      val (success2, error2) = inspectRhythmicNote(n)
      if (!success2)
        failed(error2.get)
    })
    
    success()
  }
  
  def inspectNoteHarmony(note:Note): (Boolean, Option[String]) = {
    // Test ambitus
    if (!params.global.ambitus.contains(note.getKey.getMidiKey())) {
      return failed("Out of ambitus")
    }

    // Test percentage of notes in chord

    // Test notes are in the harmony progression or scale
    val harmony:HarmonicDefinition = params.global.harmonicProgression.getHarmonyForTheTimePosition(note.getRhythmicNote.getStart)
    if (!harmony.scale.isIn(note.getKey.getScaleNote))
      return failed(note.getKey.getScaleNote + " is not in the scale " + harmony.scale)
    
    success()
  }
  
  def inspectRhythmicNote(note:Note): (Boolean, Option[String]) = {

    success()
  }
  
  private def failed(error:String):(Boolean, Option[String]) = {
    (false, Some(error))
  }
  
  private def success():(Boolean, Option[String]) = {
    (true, None)
  }
  
}
