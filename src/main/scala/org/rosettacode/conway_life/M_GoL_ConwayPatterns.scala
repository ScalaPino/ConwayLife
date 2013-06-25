package org.rosettacode.conway_life
import scala.language.implicitConversions

object ConwayPatterns {
  // Lists for all patterns available
  def stillLives = List(block, beehive, loaf, boat) map ((_, 1))
  def oscillators = (eight, 8) :: (pulsar, 3) :: oscillators2.map((_, 2))
  def oscillators2 = List(blinker, toad, beacon)
  def oscillators3 = List(pulsar)
  def oscillators8 = List(eight)
  def spaceships = List(glider, LWSS) map ((_, 4))
  def methuselahs = List((diehard, 130), (acorn, 5206), (rPentomino, 1103))
  def guns = List(gosperGun)

  // Still Lives patterns
  val block = """|
                 | XX
                 | XX
                 |"""
  val beehive = """|
                   |  XX
                   | X  X
                   |  XX
                   |"""
  val loaf = """|
                |  XX
                | X  X
                |  X X
                |   X
                |"""
  val boat = """|
                | XX
                | X X
                |  X
                |"""

  // Oscillators patterns
  val blinker = """|
                   |
                   | XXX
                   |
                   |"""
  val toad = """|
                |
                |  XXX
                | XXX
                |
                |"""
  val beacon = """|
                  | XX
                  | XX
                  |   XX
                  |   XX
                  |"""
  val pulsar = """|
                  |
                  |    XXX   XXX
                  |
                  |  X    X X    X
                  |  X    X X    X
                  |  X    X X    X
                  |    XXX   XXX
                  |
                  |    XXX   XXX
                  |  X    X X    X
                  |  X    X X    X
                  |  X    X X    X
                  |
                  |    XXX   XXX
                  |
                  |"""
  val eight = """|
                 |XXX
                 |XXX
                 |XXX
                 |   XXX
                 |   XXX
                 |   XXX
                 |"""

  // Spaceship patterns
  val glider = """|
                  |   X
                  | X X
                  |  XX
                  |"""
  val LWSS = """|
                |
                |  XXXX
                | X   X
                |     X
                | X  X
                |"""

  // Methuselah patterns
  val diehard = """|
                   |       X
                   | XX
                   |  X   XXX
                   |"""

  val acorn = """|
                 |  X
                 |    X
                 | XX  XXX
                 |"""

  val rPentomino = """|
                      | XX
                      |  XX
                      |  X
                      |"""

  // Guns
  val gosperGun = """|
                     |                        X
                     |                      X X
                     |            XX      XX            XX
                     |           X   X    XX            XX
                     |XX        X     X   XX
                     |XX        X   X XX    X X
                     |          X     X       X
                     |           X   X
                     |            XX
                     |"""

  // Helper methods
  // Enable constructing sets of coordinates from string patterns.
  implicit def coordsFromPattern(pattern: String) = for {
    (tupleCharCommaXCharPos, lineNumber) <- pattern.stripMargin.lines.map(_.zipWithIndex).zipWithIndex
    (char, xCharPos) <- tupleCharCommaXCharPos
    if char != ' '
  } yield Cell(xCharPos, lineNumber)
  
   // Move a set of coordinates to a point
  def moveTo(pattern: String, to: Cell) = (pattern: Iterator[Cell]) map (_ + to)
  def moveTo(coords: Iterator[Cell], to: Cell) = coords map (_ + to)
  def moveTo(coords: Traversable[Cell], to: Cell) = coords map (_ + to)
} // object ConwayPatterns

