package com.cabrol.francois.mural.generator.rulebased.sequential.phrase

import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.ScaleNote
import com.cabrol.francois.mural.generator.rulebased.parameters.Parameters
import com.cabrol.francois.mural.tools.RandomUtils

/**
 * @author  Francois Cabrol <francois.cabrol@live.fr>
 * @since   2014-09-29
 */
object sequenceOfPhraseGeneratorsFactory {


  def randomPhraseDuration(sequenceLength:Int, startingPoint:Float):Float = {
    val maxLenght = BigDecimal((sequenceLength - startingPoint)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat
    //println(maxLenght)
    Math.round(RandomUtils.randomFloatBetween(1, maxLenght));
  }

  def randomSilenceDurationBetweenTwoPhrases(sequenceLength:Int, endingPoint:Float):Float = {
    val maxLenght = BigDecimal((sequenceLength - endingPoint)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat
    RandomUtils.randomFloatBetween(1, maxLenght)
  }

  def randomEndingPoint(sequenceLength:Int, startingPoint:Float):Float = {
    val endingPoint = startingPoint + randomPhraseDuration(sequenceLength, startingPoint: Float)
    if(endingPoint > sequenceLength)
      sequenceLength
    else
      Math.round(endingPoint)
  }

  def create(parameters:Parameters):List[PhraseGenerator] = {

    def createFirstPhrase:PhraseGenerator = {
      createNewPhrase(0)
    }

    def createPhrase(lastPhraseGenerator:PhraseGenerator):PhraseGenerator = {
      val startingPoint = Math.round(lastPhraseGenerator.endingPoint + randomSilenceDurationBetweenTwoPhrases(parameters.global.sequenceLenght, lastPhraseGenerator.endingPoint))
      createNewPhrase(startingPoint)
    }

    def createNewPhrase(startingPoint: Float):PhraseGenerator = {
      val endingPoint = randomEndingPoint(parameters.global.sequenceLenght, startingPoint)
      val startingNote:ScaleNote = parameters.global.harmonicProgression.getHarmonyForTheTimePosition(startingPoint).chord.getRoot
      val endingNote:ScaleNote = parameters.global.harmonicProgression.getHarmonyForTheTimePosition(endingPoint).chord.getScaleNote(2)
      new PhraseGenerator(startingPoint, endingPoint, startingNote, endingNote, parameters)
    }

    def addPhrase(phrases:List[PhraseGenerator]):List[PhraseGenerator] = {
      if(phrases.last.endingPoint >= (parameters.global.sequenceLenght - 0.5))
        phrases
      else
        addPhrase(phrases ::: List(createPhrase(phrases.last)))
    }

    addPhrase(List(createFirstPhrase))
  }
}
