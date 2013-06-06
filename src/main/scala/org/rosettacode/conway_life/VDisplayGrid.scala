package org.rosettacode.conway_life

import scala.language.postfixOps

object DisplayGrid {
  val colQtd = 50
  val rowQtd = 50
  lazy val center = Life(colQtd / 2, rowQtd / 2)

  /*
   * Create a map with all visual Cells, key is a Life coordinate, value is Cell.         
   * 
   * "flatten: _*" = Repeated parameters “This notation tells the compiler to pass each
   * element of list as its own argument to echo, rather than all of it as a single argument.”
   */

  val cells: Map[Life, Cell] =
    Map(List.tabulate(colQtd, rowQtd)((x, y) => Life(x, y)
      -> (new Cell( // the callback, toggle dead/alive
        { cell => this(Life(x, y)) = !cell.isAlive }))).flatten: _*)

  private var internalGrid = Game.empty
  def displayable(coord: Life) = (0 until colQtd contains coord.x) && (0 until rowQtd contains coord.y)

  def clear {
    internalGrid.population foreach (this(_) = false)
  }

  def grid = internalGrid
  def grid_=(g: Game) = {
    //    println("Present:" + internalGrid)
    //    println("Next: " + g.population)
    //    println("To clear: " + internalGrid.filterNot(g.population contains))
    internalGrid filterNot (g.population contains) foreach (this(_) = false)
    g.population foreach (this(_) = true)
  }

  def apply(coord: Life) = internalGrid contains coord
  def update(coord: Life, setAlive: Boolean): Unit =
    if (setAlive) {
      internalGrid += coord
      if (displayable(coord)) cells(coord).alive
    } else {
      internalGrid -= coord
      if (displayable(coord)) cells(coord).dead
    }
  def switch(coord: Life) = this(coord) = !this(coord)

} // object DisplayGrid
