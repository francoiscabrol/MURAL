package com.cabrol.francois.mural.generator.rulebased.sequential.phrase

import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.ScaleNote
import com.cabrol.francois.mural.generator.rulebased.parameters.Parameters
import com.cabrol.francois.mural.tools.RandomUtils

/**
 * Create a sequence of PhraseGenerator objects which ones will allow to generate each phrases of the sequence
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   2014-09-29
 */
object sequenceOfPhraseGeneratorsFactory {

  /**
   * Generate a random phrase duration
   * @param sequenceLength
   * @param startingPoint
   * @return a random phrase duration
   */
  private def randomPhraseDuration(sequenceLength:Int, startingPoint:Float):Float = {
    // a round is done on maxDuration from the second decimal
    val maxDuration = BigDecimal((sequenceLength - startingPoint)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat
    Math.round(RandomUtils.floatBetween(1, maxDuration));
  }

  /**
   *
   * @param sequenceLength
   * @param endingPoint
   * @return a random duration
   */
  private def randomGapDurationBetweenTwoPhrases(sequenceLength:Int, endingPoint:Float):Float = {
    // a round is done on maxDuration from the second decimal
    val maxDuration = BigDecimal((sequenceLength - endingPoint)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat
    RandomUtils.exponentialDistributionBetween(0, 1).toFloat
  }

  /**
   * Calculate a phrase ending time point from a random phrase duration
   * @param sequenceLength
   * @param startingPoint
   * @return a random phrase ending time point
   */
  private def randomEndingPoint(sequenceLength:Int, startingPoint:Float):Float = {
    val endingPoint = startingPoint + randomPhraseDuration(sequenceLength, startingPoint)
    if(endingPoint > sequenceLength)
      sequenceLength
    else
      Math.round(endingPoint)
  }

  /**
   * Create a sequence of PhraseGenerator objects which ones will allow to generate each phrases of the sequence
   * @param parameters represent the generation parameters
   * @return the sequence of PhraseGenerator objects
   */
  def create(parameters:Parameters):List[PhraseGenerator] = {

    /**
     * Create a new phrase generator
     * @param startingPoint is the position of the first note in time
     * @return the new PhraseGenerator object
     */
    def createPhraseGenerator(startingPoint: Float):PhraseGenerator = {
      val endingPoint = randomEndingPoint(parameters.global.sequenceLenght, startingPoint)
      val startingNote:ScaleNote = parameters.global.harmonicProgression.getHarmonyForTheTimePosition(startingPoint).scale.getScaleNote(0)
      val endingNote:ScaleNote = parameters.global.harmonicProgression.getHarmonyForTheTimePosition(endingPoint).chord.getScaleNote(2)
      new PhraseGenerator(startingPoint, endingPoint, startingNote, endingNote, parameters)
    }

    /**
     * Create the first phrase generator
     * @return
     */
    def createFirstPhraseGenerator:PhraseGenerator = {
      createPhraseGenerator(0)
    }

    /**
     * Create any new phrase generator instance except for the first one of the sequence
     * @param lastPhraseGenerator is the last phrase generator created
     * @return
     */
    def createAnyPhraseGenerator(lastPhraseGenerator:PhraseGenerator):PhraseGenerator = {
      val startingPoint = Math.round(lastPhraseGenerator.endingPoint + randomGapDurationBetweenTwoPhrases(parameters.global.sequenceLenght, lastPhraseGenerator.endingPoint))
      createPhraseGenerator(startingPoint)
    }

    def addPhrase(phrases:List[PhraseGenerator]):List[PhraseGenerator] = {
      // create new PhraseGenerator objects until the last phrase go out the sequence
      if(phrases.last.endingPoint >= (parameters.global.sequenceLenght - 0.5))
        phrases
      else
        addPhrase(phrases ::: List(createAnyPhraseGenerator(phrases.last)))
    }

    addPhrase(List(createFirstPhraseGenerator))
  }
}
