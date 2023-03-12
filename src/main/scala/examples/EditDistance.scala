package examples

import spire.syntax.cfor.*

object EditDistance {
  /** Computes the edit distance between two strings. */
  def distance(s: String, t: String): Int = { //write your code here
    val n = s.length
    val m = t.length
    val dist = Array.ofDim[Int](n + 1, m + 1)
    for ( i <- 1 to n ) {dist(i)(0) = i }
    for ( i <- 1 to m ) {dist(0)(i) = i }
    for ( i <- 1 to n ) {
      for ( j <- 1 to m ) {
        val north = dist(i - 1)(j) + 1
        val west = dist(i)(j - 1) + 1
        val schar = s.charAt(i - 1)
        val tchar = t.charAt(j - 1)
        val nw = if (schar == tchar) {
          dist(i - 1)(j - 1)
        } else {
          dist(i - 1)(j - 1) + 1
        }
        val min = Math.min(Math.min(north, west), nw)
        dist(i)(j) = min
      }
    }
    dist(n)(m)
  }

  /** Computes the edit distance between two strings (optimized version). */
  def distanceOpt(s: String, t: String): Int = { //write your code here
    val n = s.length
    val m = t.length
    val dist = Array.ofDim[Int](n + 1, m + 1)
    cforRange(1 to n) { i => dist(i)(0) = i }
    cforRange(1 to m) { i => dist(0)(i) = i }
    cforRange(1 to n) { i =>
      cforRange(1 to m) { j =>
        val north = dist(i - 1)(j) + 1
        val west = dist(i)(j - 1) + 1
        val schar = s.charAt(i - 1)
        val tchar = t.charAt(j - 1)
        val nw = if (schar == tchar) {
          dist(i - 1)(j - 1)
        } else {
          dist(i - 1)(j - 1) + 1
        }
        val min = Math.min(Math.min(north, west), nw)
        dist(i)(j) = min
      }
    }
    dist(n)(m)
  }
}
