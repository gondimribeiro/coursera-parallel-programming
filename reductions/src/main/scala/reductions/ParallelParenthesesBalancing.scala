package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000

    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }

    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var i: Int = 0
    var count: Int = 0
    while (i < chars.length && count >= 0) {
      if (chars(i) == '(') count = count + 1
      if (chars(i) == ')') count = count - 1
      i = i + 1
    }
  
    count == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =
    
    def traverse(from: Int, until: Int): (Int, Int) = {
      var openCount: Int = 0
      var closeCount: Int = 0
      
      var i: Int = from
      while (i < until) {
        if (chars(i) == '(') openCount = openCount + 1
        else if (openCount > 0 && chars(i) == ')') openCount = openCount - 1
        else if (chars(i) == ')') closeCount = closeCount - 1
        i = i + 1
      }
      
      (closeCount, openCount)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
     
      if (until - from <= threshold) {
        val (closeCount, openCount) = traverse(from, until)
        (closeCount, openCount)
      }
        
      else {
        val mid = from + (until - from) / 2

        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))
        val (leftCloseCount, leftOpenCount) = left
        val (rightCloseCount, rightOpenCount) = right
        
        if (leftOpenCount + rightCloseCount < 0) {
          val closeCount = leftOpenCount + rightCloseCount + leftCloseCount
          val openCount = rightOpenCount

          (closeCount, openCount)
        }
        else {
          val closeCount = leftCloseCount
          val openCount = leftOpenCount + rightCloseCount + rightOpenCount

          (closeCount, openCount)
        }
      }
    }

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!

