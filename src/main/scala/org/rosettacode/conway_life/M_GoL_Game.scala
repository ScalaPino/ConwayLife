package org.rosettacode.conway_life
import scala.language.postfixOps
import scala.language.implicitConversions

/**
 * Basic virtual game cell contains own x,y coordinate and neighbors.
 */
class Cell(val x: Int, val y: Int) {

  // A memoized list of all neighbors of a coordinate
  lazy val vicinityCells = for {
    xo <- Cell.offsets
    yo <- Cell.offsets if (xo != 0 || yo != 0)
  } yield Cell(x + xo, y + yo)

  // Coordinates can be used as offsets
  def +(c: Cell) = Cell(x + c.x, y + c.y)
  def -(c: Cell) = Cell(x - c.x, y - c.y)

  /* override def equals(other: Any):Boolean 
   *  is unnecessary because the equal comparison is default by object identity (address)
   *  as inherited from Any. This works great because for a unique key x,y there is also
   *  an unique identity. This is made  by the apply method.
   */

  override def toString = f"Life($x%d, $y%d)"
} // class Cell

object Cell {
  private val offsets = -1 to 1 // For neighbor selection
  val cache = collection.mutable.HashMap.empty[(Int, Int), Cell].par //34224ms

  // An Ordering for coordinates which sorts by the X coordinate
  val xOrdering = Ordering.fromLessThan((_: Cell).x < (_: Cell).x)
  // An Ordering for coordinates which sorts by the Y coordinate
  val yOrdering = Ordering.fromLessThan((_: Cell).y < (_: Cell).y)

  // The Cell factory which checks if the new cell already exists.
  // If so, copy the cached one.
  // It makes equality and groupBy (identity) possible.
  def apply(x: Int, y: Int): Cell = { // getOrElseUpdate not available for .par
    cache.get(x, y) match {
      case Some(v) => v
      case None    => val d = new Cell(x, y); cache((x, y)) = d; d
    }
  }

  // Any Tuple2[Int, Int] can be used as a Cell through this implicit conversion.  
  implicit def coordFromTuple(t: (Int, Int)) = apply(t._1, t._2)
} // object Cell

/////////////////////////////////////////////////////////////////////////////
/**
 * Basic virtual game contains own x,y coordinates of living cells.
 *
 */
class Game(val population: Set[Cell], rulestringS: Set[Int], rulestringB: Set[Int])
    extends Set[Cell] {

  // Abstract methods that need to be defined as a Set
  def contains(elem: Cell): Boolean = population contains elem
  def iterator: Iterator[Cell] = population.iterator
  def +(elem: Cell): Game = if (population contains elem) this else Game(population + elem)
  def -(elem: Cell): Game = if (population contains elem) Game(population - elem) else this

  def nextGeneration = Game(Game.nextGeneration(population))

  /**
   * Return a string with the representation of this generation on a window
   * defined by its upper-left and lower-right coordinates.
   */
  def windowToString(upperLeft: Cell, lowerRight: Cell) = {
      def toChar(c: Cell) = if (population contains c) 'X' else ' '
      def toRow(y: Int) = for (x <- upperLeft.x to lowerRight.x) yield toChar(Cell(x, y))
      def toMatrix = for (y <- upperLeft.y to lowerRight.y by -1) yield toRow(y).mkString
    toMatrix mkString "\n"
  }

  /**
   * This generation's upper left corner Cell
   */
  lazy val upperLeft = {
    val x = population min Cell.xOrdering x;
    val y = population max Cell.yOrdering y;
    Cell(x, y)
  }

  /**
   * This generation's lower right corner Cell
   */
  lazy val lowerRight = {
    val x = population max Cell.xOrdering x;
    val y = population min Cell.yOrdering y;
    Cell(x, y)
  }

  /**
   * Move the pattern without altering its disposition
   */
  def move(center: Cell): Game = {
    val offset = Cell(
      upperLeft.x + (lowerRight.x - upperLeft.x) / 2 - center.x,
      lowerRight.y + (upperLeft.y - lowerRight.y) / 2 - center.y)
    Game(population map (_ - offset))
  }

  /**
   * Re-center at 0, 0
   */
  def recenter: Game = move(Cell(0, 0))

  override def equals(other: Any) = other match {
    case that: Game => Game.this.population == that.population
    case _          => false
  }
} // class Game

object Game {
  def apply(lives: Iterator[Cell]): Game = apply(lives.toSeq)
  def apply(lives: Traversable[Cell]): Game = apply(lives.toSet)
  def apply(alive: Set[Cell]) = new Game(alive, Set(2, 3), Set(3))
  def apply() = new Game(Set.empty[Cell], Set(2, 3), Set(3))
  def empty = apply()

  /**
   * The next generation is composed of babies from fecund
   *  neighborhoods and adults on stable neighborhoods.
   */
  def nextGeneration(population: Set[Cell],
                     rulestringS: Set[Int] = Set(2, 3),
                     rulestringB: Set[Int] = Set(3)): Set[Cell] = {
    /**
     * A map containing all coordinates that are neighbors of Cells which
     * are alive, together with the number of Cells it is neighbor of.
     */
    val neighbors =
      population.toList flatMap (_.vicinityCells) groupBy (identity) map {
        case (coor, list) => (coor, list.length)
      }

      // Filter all neighbors for desired characteristics

      // Criterion of rulestring Birth
      def reproductions = neighbors.filter(a => rulestringB contains a._2).keys
      def survivors = neighbors.filter(a => (rulestringS contains a._2)
        // Criterion of Survivors rulestring 
        && (population contains a._1)).keySet // Previous living Cell

    survivors ++ reproductions
  }
} // object Game

