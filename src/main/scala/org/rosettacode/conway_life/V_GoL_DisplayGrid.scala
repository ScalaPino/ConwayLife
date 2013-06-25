package org.rosettacode.conway_life

import scala.language.postfixOps

object DisplayGrid {
  val colQtd = 80
  val rowQtd = 40
  lazy val center = Cell(colQtd / 2, rowQtd / 2)

  /*
   * Create a map with all visual Cells, key is a Life coordinate, value is Cell.         
   * 
   * "flatten: _*" = "Spread operator" "This notation tells the compiler to pass each
   * element of list as its own argument to echo, rather than all of it as a single argument."
   */

  val cells =
    Map(Seq.tabulate(colQtd, rowQtd)((x, y) =>
      Cell(x, y) -> (new VCell( // the callback, toggle dead/alive
        { cell => this(Cell(x, y)) = !cell.isAlive }))).flatten: _*)

  private var internalGrid = Game.empty
  def displayable(coord: Cell) = (0 until colQtd contains coord.x) && (0 until rowQtd contains coord.y)

  def clear {
    internalGrid.population foreach (this(_) = false)
  }

  def grid = internalGrid
  def grid_=(g: Game) = {
    // VCell to clear
    internalGrid filterNot (g.population contains) foreach (this(_) = false)
    g.population foreach (this(_) = true)
  }

  def apply(coord: Cell) = internalGrid contains coord
  def update(coord: Cell, setAlive: Boolean): Unit =
    if (setAlive) {
      internalGrid += coord
      if (displayable(coord)) cells(coord).alive
    } else {
      internalGrid -= coord
      if (displayable(coord)) cells(coord).dead
    }

  def switch(coord: Cell) = this(coord) = !this(coord)

} // object DisplayGrid
