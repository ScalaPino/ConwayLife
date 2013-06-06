package org.rosettacode.conway_life
import scala.language.implicitConversions

class Life(val x: Int, val y: Int) {

  private def offsetsOf(n: Int) = Life.offsets map (_ + n)

  // A memoized list of all neighbors of a coordinate
  lazy val neighborCells = for {
    xn <- offsetsOf(x)
    yn <- offsetsOf(y) if (x, y) != (xn, yn)
  } yield Life(xn, yn)

  // Coordinates can be used as offsets
  def +(c: Life) = Life(x + c.x, y + c.y)
  def -(c: Life) = Life(x - c.x, y - c.y)

  /*  override def equals(other: Any):Boolean = other match {
    case that: Life => (this.x == that.x) && (this.y == that.y)
    case _ => false
  }
*/ // override def hashCode = ((x * 41) + y) * 41 + 41
  // without cache 44995 ms

  //  // ConwayTester with hash 38940 ms
  //  // ConwayTester w/o  hash 34358 ms
  override def toString = f"Life($x%d, $y%d)"
}

object Life {
  private val offsets = -1 to 1
  private val cache = new scala.collection.mutable.HashMap[(Int, Int), Life]

  def apply(x: Int, y: Int): Life = {
    cache getOrElseUpdate ((x, y), new Life(x, y))
  }

  /**
   * Any Tuple2[Int, Int] can be used as a Life
   * through this implicit conversion.
   */
  implicit def coordFromTuple(t: (Int, Int)) = apply(t._1, t._2)

  // An Ordering for coordinates which sorts by the X coordinate
  val xOrdering = Ordering.fromLessThan((_: Life).x < (_: Life).x)
  // An Ordering for coordinates which sorts by the Y coordinate
  val yOrdering = Ordering.fromLessThan((_: Life).y < (_: Life).y)
}
