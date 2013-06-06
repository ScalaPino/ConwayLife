package org.rosettacode.conway_life
import scala.language.postfixOps

class Game(val population: Set[Life]) extends Set[Life] {
  import Game._

  // Abstract methods that need to be defined as a Set
  def contains(elem: Life): Boolean = population contains elem
  def iterator: Iterator[Life] = population.iterator
  def +(elem: Life): Game = if (population contains elem) this else Game(population + elem)
  def -(elem: Life): Game = if (population contains elem) Game(population - elem) else this

  /**
   * A map containing all coordinates that are neighbors of a cell which
   * is alive, together with the number of alive cells it is neighbor of.
   */
  lazy val neighbors =
    population.toList flatMap (_.neighborCells) groupBy (identity) map {
      case (coor, list) => (coor, list.size)
    }

  // Filter all neighbors for desired characteristics
  //def neighborhood(filter: Filter) = for (filter(coord) <- neighbors) yield coord
  def reproductions = neighbors.collect { case (c, 3) => (c) }
  def survivors = population & (neighbors.collect { case (c, 2 | 3) => (c) }).toSet

  /**
   * The next generation is composed of babies from fecund neighborhoods and adults on stable
   * neighborhoods.
   */
  def nextGeneration = Game(survivors ++ reproductions)

  /**
   * Return a string with the representation of this generation on a window
   * defined by its upper-left and lower-right coordinates.
   */
/*  def windowToString(upperLeft: Life, lowerRight: Life) = {
    def toChar(c: Life) = if (population contains c) 'X' else ' '
    def toRow(y: Int) = for (x <- upperLeft.x to lowerRight.x) yield toChar(Life(x, y))
    def toMatrix = for (y <- upperLeft.y to lowerRight.y by -1) yield toRow(y).mkString
    toMatrix mkString "\n"
  }
*/
  /**
   * This generation's upper left corner
   */
  lazy val upperLeft = {
    val x = population min Life.xOrdering x;
    val y = population max Life.yOrdering y;
    Life(x, y)
  }

  /**
   * This generation's lower right corner
   */
  lazy val lowerRight = {
    val x = population max Life.xOrdering x;
    val y = population min Life.yOrdering y;
    Life(x, y)
  }

  /**
   * Recenter the pattern without altering its disposition
   */
  def recenter(center: Life) = {
    val offset = Life(
      upperLeft.x + (lowerRight.x - upperLeft.x) / 2 - center.x,
      lowerRight.y + (upperLeft.y - lowerRight.y) / 2 - center.y)
    Game(population map (_ - offset))
  }

  /**
   * Recenter at 0, 0
   */
  def recenter: Game = recenter(Life(0, 0))

  override def equals(other: Any) = other match {
    case that: Game => Game.this.population == that.population
    case _ => false
  }
  override def hashCode = population.hashCode
  //override def toString = if (alive.isEmpty) "empty" else windowToString(upperLeft, lowerRight)
}

object Game {
  def apply(lives: Iterator[Life]): Game = apply(lives.toSeq)
  def apply(lives: Traversable[Life]): Game = apply(lives.toSet)
  def apply(alive: Set[Life]) = new Game(alive)
  def apply() = new Game(Set.empty[Life])
  def empty = apply()
}

