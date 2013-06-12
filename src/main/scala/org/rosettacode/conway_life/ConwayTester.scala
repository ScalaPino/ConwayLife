package org.rosettacode.conway_life

import scala.language.postfixOps

object ConwayTester extends App {
  import ConwayPatterns._
  val MaxGenerations = 5500 // Give up at MaxGenerations to avoid spending too much time on errors
  val WindowSize = 10 // To check for stable populations, use a window this big
  import Cell.{ xOrdering, yOrdering }

  /**
   *  Return an iterator for the generations of a starting pattern
   */
  def conwayIterator(first: Game) = Iterator.iterate(first)(_.nextGeneration)

  /**
   * Return the period (number of different generations) for oscillators
   */
  def getPeriod(first: Game) = {
    val it = conwayIterator(first)
    it.next // drop first generation
    (it take MaxGenerations takeWhile (_ != first) length) + 1
  }

  /**
   * Return the period (number of different generations, ignoring offset) for
   * spaceships.
   */
  def getSpaceshipPeriod(first: Game) = {
    val it = conwayIterator(first) map (_.recenter)
    it.next // drop first generation
    (it take MaxGenerations takeWhile (_ != first) length) + 1
  }

  /**
   * Return the number of generations until the population of a pattern
   * stabilizes. This test only checks a window of generations for
   * population size, as the test for spaceships won't work when multiple
   * spaceships are present.
   */
  def getUnstableGenerations(first: Game) = (
    conwayIterator(first)
      take MaxGenerations
      map (_.size)
      sliding WindowSize
      map (_.distinct.length)
      takeWhile (_ > 1) length)

  /**
   * Return the first generation, properly centered, for a given pattern
   * as represented by a string.
   */
  def initPattern(pattern: String) =
    Game(pattern: Iterator[Cell]).recenter

  /**
   * For each pattern passed, apply a function which will measure some characteristic
   * of the generations of that pattern, and assert it is equal to expected value.
   */
  def testHarness(patterns: Traversable[(String, Int)], test: Game => Int, msg: String) =
    assert(patterns forall { case (pattern, period) => test(initPattern(pattern)) == period }, msg)

  // Available tests
  def testStillLives = testHarness(stillLives, getPeriod _, "Failure in still lives")
  def testOscillators = testHarness(oscillators, getPeriod _, "Failure in oscillators")
  def testSpaceships = testHarness(spaceships, getSpaceshipPeriod _, "Failure in spaceships")
  def testMethuselahs = testHarness(methuselahs, getUnstableGenerations _, "Failure in methuselahs")

  /**
   * Do all available tests
   */
  def testAll {
    testStillLives
    testOscillators
    testSpaceships
    testMethuselahs
  }

  /**
   * Do all available tests, and print three generations of a blinker on a 3x3 window.
   */
  {
    val upperLeft = Cell(-1, 1)
    val lowerRight = Cell(1, -1)
    testAll
    println("Passed all tests. Printing three generations of blinker:")
    conwayIterator(initPattern(blinker)).zipWithIndex.take(3).toList zip List("st", "nd", "rd") foreach {
      case ((generation, nth), suffix) =>
        println((nth + 1) + suffix + " generation: \n" + generation.windowToString(upperLeft, lowerRight) + "\n")
    }
  }
}

/*
Paralelizar:
List((Set(C1), Set(Neighbors de C1)), alive - C1
Para todo Cx tal que Cx pertence � Neighbors de C1 e � alive,
adicione Cx � Set(C1),
substitua Neighbors de C1 por Neighbors Cx &~ Neighbors de C1
Repita at� n�o existir Cx
Crie uma lista separada
Pegue mais um elemento de alive
Repita

def divide(set: Set[Coord], neighbors: Set[Coord], remaining: Set[Coord]) =
  if (remaining.isEmpty) {
    List(set)
  } else {
    val (joined, separate) = remaining partition (neighbors contains _)
    if (joined.isEmpty)
      joined :: divide(remaining.head, remaining.head.neighbors, remaining.tail)
    else
      divide(set ++ joined, joined.neighbors &~ neighbors, separate)
  }
}

Grupos: alive: Set[Coord], right: Int, left: Int, up: Int, down: Int

A cada nextGeneration, para todos grupos A, B, se A.right == B.left || A.up == B.down, junte ambos

NextGeneration:

Em paralelo, computa o alive de cada grupo
Aplica divide em cada grupo
Calcula left, right, up, down de cada grupo
Aplica join em todos os grupos

Manter grupos centrados, com offset. Cachear grupos, manter offsets separados.

*/
