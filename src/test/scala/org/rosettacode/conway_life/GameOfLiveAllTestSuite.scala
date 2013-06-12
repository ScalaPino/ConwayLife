package org.rosettacode.conway_life

import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

// @SuiteClasses({AllTests})
class AllTests extends AssertionsForJUnit {

  var universe: Game = _

  @Before def initialize() {
    universe = Game(Set())
  }

  @Test def verifyLife() { // Uses ScalaTest assertions
    val x = 4
    val y = 5

    assert(Cell(x, y) === Cell(x, y))

    val coord = Cell(x, y)
    // Test neighborhood cell generator 
    assert((coord.neighborCells.toSet &~
      Game(Set[Cell]((x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
        (x, y - 1), (x, y + 1),
        (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)))).size === 0)

  };

  @Test def verifyGeneration() { // Uses ScalaTest assertions
    assert(universe.isEmpty)
    universe += Cell(1, 2)
    assert(universe.size === 1)
    // Test on duplicates
    universe += Cell(1, 2)
    assert(universe.size === 1)

    universe += Cell(2, 2); universe += Cell(3, 2)
    assert(universe === Game(Set[Cell]((2, 2), (1, 2), (3, 2))))

    assert(universe.neighbors === Map(Cell(0, 2) -> 1, Cell(3, 1) -> 2, Cell(4, 1) -> 1,
      Cell(0, 3) -> 1, Cell(1, 1) -> 2, Cell(3, 2) -> 1,
      Cell(1, 3) -> 2, Cell(2, 2) -> 2, Cell(4, 2) -> 1,
      Cell(0, 1) -> 1, Cell(3, 3) -> 2, Cell(2, 3) -> 3,
      Cell(1, 2) -> 1, Cell(2, 1) -> 3, Cell(4, 3) -> 1))

    assert(universe.reproductions === List(Cell(2, 1), Cell(2, 3)))

    assert(universe.survivors === Set(Cell(2, 2)))

    assert(universe.nextGeneration === Game(Set(Cell(2, 1), Cell(2, 2), Cell(2, 3))))

    // intercept[StringIndexOutOfBoundsException] {
    //   "concise".charAt(-1)
    // }
  }
}
