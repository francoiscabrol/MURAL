package main.scala.com.cabrol.francois.mural

import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.{ Scale, Chord }
import com.cabrol.francois.libjamu.musictheory.entity.note.Note
import com.cabrol.francois.melvi.factory.MelodyVisualiserFactory
import com.cabrol.francois.melvi.view.VisualiserView
import java.awt.{ Point, Toolkit }
import com.cabrol.francois.mural.generator.rulebased.Generator
import com.cabrol.francois.mural.generator.rulebased.method.Methods

import scala.collection.mutable
import com.cabrol.francois.mural.generator.rulebased.parameters._
import com.cabrol.francois.mural.generator.rulebased.parameters.HarmonicDefinition

import java.io.File
import javax.sound.midi.ShortMessage
import javax.sound.midi.ShortMessage._
import javax.sound.midi.MidiEvent
import javax.sound.midi.MidiSystem
import javax.sound.midi.Sequence
import javax.sound.midi.SysexMessage
import javax.sound.midi.MetaMessage

/**
 * Created with IntelliJ IDEA.
 * User: francois * Date: 2014-02-04
 */
object Test {

  def saveMidi(file: File, notes: List[Note], play: Boolean) = {

    val seq = new Sequence(Sequence.PPQ, 4);
    val track = seq.createTrack();

    // General MIDI sysex -- turn on General MIDI sound set
    val b = Array[Byte](0xF0.asInstanceOf[Byte], 0x7E, 0x7F, 0x09, 0x01, 0xF7.asInstanceOf[Byte]);
    var sm = new SysexMessage();
    sm.setMessage(b, 6);
    var me = new MidiEvent(sm, 0);
    track.add(me);

    // set tempo (meta event)
    var mt = new MetaMessage();
    val bt = Array[Byte](0x03, 0x00, 0x00);
    mt.setMessage(0x51, bt, 3);
    me = new MidiEvent(mt, 0);
    track.add(me);

    // set track name (meta event)
    mt = new MetaMessage();
    val trackName = new String("mural track");
    mt.setMessage(0x03, trackName.getBytes(), trackName.length());
    me = new MidiEvent(mt, 0);
    track.add(me);

    def makeEvent(comd: Int, chan: Int, one: Int, two: Int, tick: Int): MidiEvent = {
      val a = new ShortMessage();
      a.setMessage(comd, chan, one, two);
      return new MidiEvent(a, tick);
    }

    // set omni on
    track.add(makeEvent(CONTROL_CHANGE, 0, 0x7D, 0x00, 0));

    // set poly on
    track.add(makeEvent(CONTROL_CHANGE, 0, 0x7F, 0x00, 0));

    // set instrument to Piano
    track.add(makeEvent(PROGRAM_CHANGE, 0x00, 0x00, 0x00, 0));

    var maxtick = 0
    for (n <- notes) {
      println(n)
      val start = (n.getRhythmicNote.getStart * 10).asInstanceOf[Int]
      val end = start + (n.getRhythmicNote.getDuration * 10).asInstanceOf[Int]
      track.add(makeEvent(NOTE_ON, 0, n.getKey.getMidiKey,
        n.getRhythmicNote.getVelocity,
        start))
      track.add(makeEvent(NOTE_OFF, 0, n.getKey.getMidiKey,
        n.getRhythmicNote.getVelocity,
        end))
      if (maxtick < end)
        maxtick = end
    }

    // set end of track (meta event)
    mt = new MetaMessage();
    mt.setMessage(0x2F, Array[Byte](), 0);
    me = new javax.sound.midi.MidiEvent(mt, maxtick);
    track.add(me);

    // write the MIDI sequence to a MIDI file
    MidiSystem.write(seq, 1, file);

    if (play) {
      val sequencer = MidiSystem.getSequencer();
      sequencer.open();
      sequencer.setSequence(seq);
      sequencer.setTempoInBPM(220);
      sequencer.start();
    }
  }

  def main(args: Array[String]) = {
    print("Start the generator execution test")

    val parentNotes = List()
    val chords: Map[Float, HarmonicDefinition] = Map((0, HarmonicDefinition(new Chord("C"), new Scale("C major"))), (1, HarmonicDefinition(new Chord("Am"), new Scale("A minor"))))
    val hP = new HarmonicProgression(chords)
    val generationMethod = Methods.rulesBased
    val numBeatsPerBar = 4
    val numBars = 4
    val ambitus: Ambitus = new Ambitus(28, 71)
    val pSilence = 0
    val percentageOfNoteInChord = 50
    val numOfNoteAtTheSameTimeUnit = 1
    //val melodyCurb = MelodyCurb(numBeatsPerBar*numBars, 4)
    //println(melodyCurb.curb)
    val density = 1
    val variance = 0
    val global = new GlobalParameters(generationMethod, parentNotes, numBeatsPerBar, numBars, ambitus, hP, pSilence, numOfNoteAtTheSameTimeUnit, Direction.up, variance, density, 0, percentageOfNoteInChord)
    val dynamic: List[DynamicParameters] = List()
    val param = Parameters(global, dynamic, 1)

    var visuViews = mutable.MutableList[VisualiserView]()

    for (i <- 1 to 10) {
      val beforeTime = System.currentTimeMillis()
      val notes = Generator.generate(param)

      println("generation execution time:" + (System.currentTimeMillis() - beforeTime))

      saveMidi(new File(s"mural$i.mid"), notes, false)

      visuViews.+=(MelodyVisualiserFactory.create(notes))
    }
 
    def getVisuPos(index: Int): Point = {
      val screenSize = Toolkit.getDefaultToolkit().getScreenSize();
      (index match {
        case 0 => 0
        case _ => visuViews(index - 1).getX + visuViews(index - 1).getWidth
      }) match {
        case x if x + visuViews(index).getWidth > screenSize.getWidth => new Point(0, visuViews(index - 1).getY + visuViews(index - 1).getHeight)
        case x => new Point(x, visuViews(index - 1).getY)
      }
    }

    visuViews.zipWithIndex.foreach { case (visu, index) => visu.setLocation(getVisuPos(index)) }

  }

}