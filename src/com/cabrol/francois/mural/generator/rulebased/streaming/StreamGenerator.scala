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

package com.cabrol.francois.mural.generator.rulebased.streaming

import scala.actors.Actor
import com.cabrol.francois.mural.generator.rulebased.parameters.Parameters
import com.cabrol.francois.mural.generator.rulebased.sequential.MelodyCurveFactory
import com.cabrol.francois.libjamu.musictheory.entity.note.{RhythmicNote, Note}
import com.cabrol.francois.mural.tools.Debug
import com.cabrol.francois.mural.generator.rulebased.transition.TransitionalState
import scala.collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA.
 * User: francois * Date: 2013-11-24
 */

object PlayerMessages extends Enumeration {
  type PlayerMessages = Value
  val next = Value
}

class StreamGenerator(var param:Parameters) extends Actor{

  val melodyCurveFactory:MelodyCurveFactory = new MelodyCurveFactory
  var currentTick:Int = 0;
  val indentTick = 8;
  var notesHistory:FixedList[Note] = new FixedList[Note](20);

  def act() = {

    Debug.streamGenerator("Start")

    melodyCurveFactory.randomizeCurbType

    loop{
      react{
          case PlayerMessages.next => { Debug.streamGenerator("Receive a message Next"); sender ! MessageBox(generateNextEvents) }
      }
    }

  }

  def generateNextEvents:List[Note] = {
    val notes = generateNextNotes
    notes
//    notesHistory.appends(notes)
//    val midiNoteEvents:List[MidiNoteEvent] = notes.map(_.getMidiNoteEvents.asScala.toList).flatten
//    midiNoteEvents
  }

  def generateNextNotes:List[Note] = {
    if(notesHistory.isEmpty){
      val lastNote = generateFirstNote
      List(lastNote) ::: generateANewBlock(lastNote)
    }
    else
      generateANewBlock(notesHistory.last)
  }

  def generateFirstNote:Note = {
    Debug.streamGenerator("Generating the first note...")
    new TransitionalState(param, None, None, Some(new RhythmicNote(0, 0)), None, melodyCurveFactory).generateSingleNote
  }

  def generateANewBlock(lastNote:Note):List[Note] = {

    def getLastNotePos(notes:List[Note]):Float = notes.last.getRhythmicNote.getStart + notes.last.getRhythmicNote.getDuration

    def generateNewNote(lastNote:Note):Note = {
      Debug.streamGenerator("Generating another note...")
      new TransitionalState(param, Some(lastNote), None, None, None, melodyCurveFactory).generateSingleNote
    }

    def addNewNote(notes:List[Note]):List[Note] = {
      if( getLastNotePos(notes) >= (currentTick + indentTick)){
        notes
      }
      else
        addNewNote(notes ::: List(generateNewNote(notes.last)))
    }

//    def generateLastNote(lastNote:Note):Note = {
//      Debug.streamGenerator("Generating the last note...")
//      new TransitionalState(param, Some(lastNote), Some(endingNote), None, None, melodyCurveFactory).generateSingleNote
//    }

    Debug.streamGenerator("Generating a new block: " + "\n" + this.toString + "\n")
    addNewNote(List(lastNote))
  }
//  def generateNewNote(lastNote:Note):Note = {
//    //Debug.phraseGenerator("Generating another note...")
//    new TransitionalState(param, Some(lastNote), None, None, None, melodyCurveFactory).generateSingleNote
//  }

}

case class MessageBox(ls:List[Note]){}

class FixedList[A](max: Int) extends Traversable[A] {

  val list: ListBuffer[A] = ListBuffer()

  def append(elem: A) {
    if (list.size == max) {
      list.trimStart(1)
    }
    list.append(elem)
  }

  def appends(listElem: List[A]) {
    listElem.foreach(e => this.append(e))
  }

  def foreach[U](f: A => U) = list.foreach(f)

}