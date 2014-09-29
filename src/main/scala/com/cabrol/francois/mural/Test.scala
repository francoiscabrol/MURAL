package main.scala.com.cabrol.francois.mural

import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.{Scale, Chord}
import com.cabrol.francois.melvi.factory.MelodyVisualiserFactory
import com.cabrol.francois.melvi.view.VisualiserView
import java.awt.{Point, Toolkit}
import com.cabrol.francois.mural.generator.rulebased.Generator
import com.cabrol.francois.mural.generator.rulebased.method.Methods

import scala.collection.mutable
import com.cabrol.francois.mural.generator.rulebased.parameters._
import com.cabrol.francois.mural.generator.rulebased.parameters.HarmonicDefinition

/**
 * Created with IntelliJ IDEA.
 * User: francois * Date: 2014-02-04
 */
object Test {
  def main(args: Array[String]) = {
    print("Start the generator execution test")

    val parentNotes = List()
    val chords:Map[Float, HarmonicDefinition] = Map((0, HarmonicDefinition(new Chord("C"), new Scale("C major"))), (2, HarmonicDefinition(new Chord("Am"), new Scale("A minor"))))
    val hP = new HarmonicProgression(chords)
    val generationMethod = Methods.rulesBased
    val numBeatsPerBar = 4
    val numBars = 4
    val ambitus:Ambitus = new Ambitus(28, 71)
    val pSilence = 0
    val percentageOfNoteInChord = 50
    val numOfNoteAtTheSameTimeUnit = 1
    //val melodyCurb = MelodyCurb(numBeatsPerBar*numBars, 4)
    //println(melodyCurb.curb)
    val density = 1
    val variance = 0
    val global = new GlobalParameters(generationMethod, parentNotes, numBeatsPerBar, numBars, ambitus, hP, pSilence, numOfNoteAtTheSameTimeUnit, Direction.up, variance, density, 0, percentageOfNoteInChord)
    val dynamic:List[DynamicParameters] = List()
    val param = Parameters(global, dynamic, 1)
    val generator = new Generator(param)

    var visuViews = mutable.MutableList[VisualiserView]()

    for(i <- 1 to 10){
      val beforeTime = System.currentTimeMillis()
      val notes = generator.generate
      println("generation execution time:" + (System.currentTimeMillis() - beforeTime))
      visuViews.+=(MelodyVisualiserFactory.create(notes))
    }

    def getVisuPos(index:Int):Point = {
      val screenSize = Toolkit.getDefaultToolkit().getScreenSize();
      (index match {
        case 0 => 0
        case _ => visuViews(index-1).getX + visuViews(index-1).getWidth
      }) match {
        case x if x+visuViews(index).getWidth>screenSize.getWidth => new Point(0, visuViews(index-1).getY+visuViews(index-1).getHeight)
        case x => new Point(x, visuViews(index-1).getY)
      }
    }

    visuViews.zipWithIndex.foreach{ case(visu, index) => visu.setLocation(getVisuPos(index)) }

  }

}