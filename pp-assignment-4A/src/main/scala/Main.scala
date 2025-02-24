package pp202402.assign4a

import scala.annotation.tailrec
import scala.util.control.TailCalls._
import pp202402.assign4a.Lazylist.LCons
import pp202402.assign4a.Lazylist.LNil

/** Principles of Programming: Assignment 4A.
  *
  * Implement given functions, which are currently left blank. (???) **WARNING:
  * Please read the restrictions below carefully.**
  *
  * If you do not follow these, **your submission will not be graded.**
  *
  *   - Do not use the keyword `var`. Use `val` and `def` instead.
  *   - Do not use any library functions or data structures like `LazyList`,
  *     `Array`, `Range` (`1 to n`, `1 until n` ...), `fold`, `map`, `reduce` or
  *     etc.
  *   - If you want to use a data structure, create new one instead of using the
  *     library ones.
  *   - You can only use tuples, `scala.annotation.tailrec`,
  *     `scala.util.control.TailCalls._`, and `List`, `Nil`, `Cons` (to
  *     implement `toList` function) from the library.
  *   - Do not use any looping syntax of Scala (`for`, `while`, `do-while`,
  *     `yield`, ...)
  *
  * Again, your score will be zero if you do not follow these rules.
  *
  * We do not require tail-recursion explicitly for this assignment.
  *
  * All testcases for grading will include both instances of problem 1-1 and
  * functions of problem 1-2. That means, you may get 0 points for this
  * assignment if you only solve problem 1-1 (or 1-2).
  *
  * Timeout: 30 sec.
  */

object Lazylist {
  import Data.*

  /** Problem 1-1
    *
    * Implement instances of Iter and ListIF for Lazylist.
    */
  sealed abstract class Lazylist[A] {
    def get: Option[(A, Lazylist[A])]
  }
  class LNil[A]() extends Lazylist[A] {
    def get = None
  }
  class LCons[A](hd: A, _tl: => Lazylist[A]) extends Lazylist[A] {
    private lazy val tl = _tl
    def get = Some(hd, tl)
  }

  implicit def lazylistIter[A]: Iter[Lazylist, A] = new {
    def get[A](l: Lazylist[A]) = l.get

  }

  implicit def lazylistListIF[A]: ListIF[Lazylist, A] = new {
    def empty[A] = LNil[A]()
    def cons[A](a: A, l: => Lazylist[A]) = LCons[A](a, l)
    def iter[A] = lazylistIter[A]
    def head[A](l: Lazylist[A]) = {
      l.get match {
        case None       => None
        case Some(v, l) => Some(v)
      }
    }
    def tail[A](l: Lazylist[A]) = {
      l.get match {
        case None       => l
        case Some(v, l) => l
      }
    }
  }
}

object Assignment4A:
  import Data.*

  /** Problem 1-2
    *
    * Implement 3 functions below.
    *
    * You don't need to worry about stack overflow or Long type overflow. All
    * test cases for grading will be small enough.
    */

  /** fibo() is a list of fibonacci sequence.
    *
    * ex): fibo = [0,1,1,2,3,5,8,...]
    *
    * See https://en.wikipedia.org/wiki/Fibonacci_sequence for more information.
    *
    * remark : fibo[List] will cause error.
    */
  def fibo[L[_]](implicit listIF: ListIF[L, Long]): L[Long] = {
    def fibo_recursive(
        val1: Long,
        val2: Long
    ): L[Long] = {
      listIF.cons(val1 + val2, fibo_recursive(val2, val1 + val2))
    }
    val a = listIF.cons(0L, listIF.cons(1L, listIF.empty))
    listIF.cons(0L, listIF.cons(1L, fibo_recursive(0L, 1L)))
  }

  /** getN(l,n) is a list of first n elements of l.
    *
    * ex): getN([1,2,3,4...], 3) = [1,2,3]
    */
  def getN[A, L[_]](l: L[A], n: Int)(implicit listIF: ListIF[L, A]): L[A] = {
    def getN_recursive(i: Int, l2: L[A]): L[A] = {
      if (i == n) listIF.empty
      else
        listIF.cons(
          listIF.iter.get(l2).get._1,
          getN_recursive(i + 1, listIF.iter.get(l2).get._2)
        )
    }

    getN_recursive(0, l)

  }

  /** toIList(l) is a List version of given list l.
    */
  def toList[A, L[_]](
      x: L[A]
  )(implicit listIF: ListIF[L, A]): List[A] = {
    listIF.head(x) match {
      case None => listListIF.empty
      case Some(value) =>
        listListIF.cons(
          value,
          toList(listIF.tail(x))
        )
    }
  }
