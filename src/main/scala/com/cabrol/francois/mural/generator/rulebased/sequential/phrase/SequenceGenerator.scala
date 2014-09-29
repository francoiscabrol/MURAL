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

package com.cabrol.francois.mural.generator.rulebased.sequential.phrase

import com.cabrol.francois.libjamu.musictheory.entity.note.Note
import com.cabrol.francois.libjamu.musictheory.entity.scaleNote.ScaleNote
import com.cabrol.francois.mural.generator.rulebased.parameters.Parameters
import com.cabrol.francois.mural.tools.{Debug, RandomUtils}

/**
 * Created with IntelliJ IDEA.
 * User: francois
 * Date: 2013-11-14
 * Time: 13:39
 * To change this template use File | Settings | File Templates.
 */
object SequenceGenerator {

  def generateSequence(parameters : Parameters):List[Note] = {

    def phrasesGeneratorFactory:List[PhraseGenerator] = {

      def stochasticPhraseDuration(startingPoint:Float):Float = {
        val maxLenght = BigDecimal((parameters.global.sequenceLenght - startingPoint)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat
        //println(maxLenght)
        Math.round(RandomUtils.randomFloatBetween(1, maxLenght));
      }

      def stochasticSilenceBetweenTwoPhrases(endingPoint:Float):Float = {
        val maxLenght = BigDecimal((parameters.global.sequenceLenght - endingPoint)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat
        RandomUtils.randomFloatBetween(1, maxLenght)
      }

      def stochasticEndingPoint(startingPoint:Float):Float = {
        val endingPoint = startingPoint + stochasticPhraseDuration(startingPoint: Float)
        if(endingPoint > parameters.global.sequenceLenght)
          parameters.global.sequenceLenght
        else
          Math.round(endingPoint)
      }

      def firstPhrase:PhraseGenerator = {
        newPhrase(0)
      }

      def othersPhrase(lastPhraseGenerator:PhraseGenerator):PhraseGenerator = {
        val startingPoint = Math.round(lastPhraseGenerator.endingPoint + stochasticSilenceBetweenTwoPhrases(lastPhraseGenerator.endingPoint))
        newPhrase(startingPoint)
      }

      def newPhrase(startingPoint: Float):PhraseGenerator = {
        val endingPoint = stochasticEndingPoint(startingPoint)
        val startingNote:ScaleNote = parameters.global.harmonicProgression.getHarmonyForTheTimePosition(startingPoint).chord.getRoot
        val endingNote:ScaleNote = parameters.global.harmonicProgression.getHarmonyForTheTimePosition(endingPoint).chord.getScaleNote(2)
        new PhraseGenerator(startingPoint, endingPoint, startingNote, endingNote, parameters)
      }

      def addPhrase(phrases:List[PhraseGenerator]):List[PhraseGenerator] = {
        if(phrases.last.endingPoint >= (parameters.global.sequenceLenght - 0.5))
          phrases
        else
          addPhrase(phrases ::: List(othersPhrase(phrases.last)))
      }

      addPhrase(List(firstPhrase))
    }

    def generatePhrase(sequence:List[Note], phrasesGenerator:List[PhraseGenerator], i:Int) : List[Note] = {
      if(i >= phrasesGenerator.length)
        sequence
      else
        generatePhrase(sequence ::: phrasesGenerator(i).generateThePhrase, phrasesGenerator, i+1)
    }

    val p = phrasesGeneratorFactory
    for(x <- p) Debug.sequenceGenerator(x.toString)
    Debug.sequenceGenerator("---- \n")
    generatePhrase(List[Note](), p, 0)
  }

}
