package main.scala.com.cabrol.francois.mural

import java.awt.{Point, Toolkit}

import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.{Chord, Scale}
import com.cabrol.francois.melvi.factory.{GraphicsType, MelodyVisualiserFactory}
import com.cabrol.francois.melvi.view.VisualiserView
import com.cabrol.francois.mural.generator.rulebased.Generator
import com.cabrol.francois.mural.generator.rulebased.method.Methods
import com.cabrol.francois.mural.generator.rulebased.parameters._
import com.cabrol.francois.mural.tools.{Debug, Inspector}

import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: francois * Date: 2014-02-04
 */
object Test {

  def main(args: Array[String]) = {
    print("[TEST] Start the generator execution test")

    val parentNotes = List()
    val hP = {
      val chords: Map[Float, HarmonicDefinition] = Map((0, HarmonicDefinition(new Chord("C"), new Scale("C major"))), (4, HarmonicDefinition(new Chord("G"), new Scale("C major"))))
      new HarmonicProgression(chords)
    }
    val generationMethod = Methods.rulesBased
    val numBeatsPerBar = 4
    val numBars = 1
    val ambitus: Ambitus = new Ambitus(40, 90)
    val pSilence = 0
    val percentageOfNoteInChord = 50
    val numOfNoteAtTheSameTimeUnit = 1
    val density = 7
    val variance = 0
    val global = new GlobalParameters(generationMethod, parentNotes, numBeatsPerBar, numBars, ambitus, hP, pSilence, numOfNoteAtTheSameTimeUnit, Direction.both, variance, density, 0, percentageOfNoteInChord, PhraseParameters(Interval(0, 6), Interval(0, 0)))
    val dynamic: List[DynamicParameters] = List()
    val param = Parameters(global, dynamic, 1)

    var visuViews = mutable.MutableList[VisualiserView]()

    var inpector = new Inspector(param)

    for (i <- 1 to 1) {
      val beforeTime = System.currentTimeMillis()
      val notes = Generator.generate(param)

      val generationTime = (System.currentTimeMillis() - beforeTime)
      println("[TEST] generation execution time:" + generationTime)
      Debug.log("[TEST] generation execution time", generationTime.toString)

      inpector.inspect(notes)

      visuViews.+=(MelodyVisualiserFactory.create(notes, GraphicsType.pitchvstime))
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
