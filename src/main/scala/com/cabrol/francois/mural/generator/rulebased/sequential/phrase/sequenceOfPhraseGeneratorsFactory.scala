package com.cabrol.francois.mural.generator.rulebased.sequential.phrase

import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.ScaleNote
import com.cabrol.francois.mural.generator.rulebased.parameters.Parameters
import com.cabrol.francois.mural.tools.RandomUtils

/**
 * Create a sequence of PhraseGenerator objects which ones will allow to generate each phrases of the sequence
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   2014-09-29
 */
object SequenceOfPhraseGeneratorsFactory {

  /**
   * Generate a random phrase duration
   * @param sequenceLength
   * @param startingPoint
   * @return a random phrase duration
   */
  def randomPhraseDuration(sequenceLength:Int, startingPoint:Float, maxRequire:Float):Int = {
    // a round is done on maxDuration from the second decimal
    val maxDuration = BigDecimal((sequenceLength - startingPoint)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat
    val max = if (maxRequire > maxDuration) maxDuration else maxRequire
    math.ceil(RandomUtils.exponentialDistributionBetween(0, maxDuration, 0.5)).toInt
  } ensuring (_ >= 1, "The duration should be superior than 0")

  /**
   *
   * @param sequenceLength
   * @param endingPoint
   * @return a random duration
   */
  private def randomGapDurationBetweenTwoPhrases(sequenceLength:Int, endingPoint:Float, maxRequire:Float):Float = {
    // a round is done on maxDuration from the second decimal
    val maxDuration = BigDecimal((sequenceLength - endingPoint-1)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat
    val max = if (maxRequire > maxDuration) maxDuration else maxRequire
    RandomUtils.exponentialDistributionBetween(1, max, 0.5).toFloat
  }

  /**
   * Calculate a phrase ending time point from a random phrase duration
   * @param sequenceLength
   * @param startingPoint
   * @return a random phrase ending time point
   */
  private def randomEndingPoint(sequenceLength:Int, startingPoint:Float, maxRequire:Float):Float = {
    require(startingPoint < sequenceLength)
    val endingPoint:Double = startingPoint + randomPhraseDuration(sequenceLength, startingPoint, maxRequire)
    if(endingPoint > sequenceLength)
      sequenceLength
    else
      math.round(endingPoint)
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
      val endingPoint = randomEndingPoint(parameters.sequenceLenght, startingPoint, parameters.phrase.gap.max)
      val startingNoteDegree = RandomUtils.randomElement(List(0, 2, 4))
      val startingNote:ScaleNote = parameters.harmonicProgression.getHarmonyForTheTimePosition(startingPoint).scale.getScaleNote(startingNoteDegree)
      val endingNoteDegree = RandomUtils.randomElement(List(0, 1, 2, 4, 6))
      val endingNote:ScaleNote = parameters.harmonicProgression.getHarmonyForTheTimePosition(endingPoint).chord.getScaleNote(endingNoteDegree)
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
      val startingPoint = Math.round(lastPhraseGenerator.endingPoint + randomGapDurationBetweenTwoPhrases(parameters.sequenceLenght, lastPhraseGenerator.endingPoint, parameters.phrase.gap.max))
      createPhraseGenerator(startingPoint)
    }

    def addPhrase(phrases:List[PhraseGenerator]):List[PhraseGenerator] = {
      // create new PhraseGenerator objects until the last phrase go out the sequence
      if (phrases.last.startingPoint >= (parameters.sequenceLenght - 1)) // if the last phrase start after the sequence end
        phrases.dropRight(1)                                                    // return the sequence without the last phrase
      else if (phrases.last.endingPoint >= (parameters.sequenceLenght - 1)) //if the last phrase end after the sequence end
        phrases                                                                    // return the sequence // TODO should modify the last phrase to fit in the sequence?
      else
        addPhrase(phrases ::: List(createAnyPhraseGenerator(phrases.last)))
    }

    addPhrase(List(createFirstPhraseGenerator))
  }
}
