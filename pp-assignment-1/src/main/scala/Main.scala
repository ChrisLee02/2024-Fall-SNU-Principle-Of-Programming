package pp202402.assign1

import scala.annotation.tailrec
import scala.util.control.TailCalls._
import scala.compiletime.ops.boolean

/** Principles of Programming: Assignment 01.
  *
  * Implement the given functions, which are currently left blank. (???)
  *
  * **WARNING:Please read the restrictions below carefully.** If you do not follow these, **your
  * submission will not be graded.**
  *
  *   - Do not use the keyword `var`. Use `val` and `def` instead.
  *   - Do not use any library functions or data structures like `List`, `Array`, `Range` (`1 to n`,
  *     `1 until n`, ...), `fold`, `map`, `reduce` etc.
  *   - You may only use tuples, `scala.annotation.tailrec`, `scala.util.control.TailCalls._` from
  *     the library.
  *   - Do not use any looping syntax of Scala (`for`, `while`, `do-while`, `yield`, etc.).
  *
  * Again, your score will be zero if you do not follow these rules.
  *
  * Note that these rules will be gradually relaxed in future assignments.
  *
  * For problems except problem 1-1, some test cases will require optimizations (i.e., large inputs)
  * including tail call optimizations and the others will not (i.e., small inputs).
  *
  * So, you will get partial point if you submit a correct program without proper optimizations.
  *
  * See /src/test/scala/TestSuite.scala for some test cases (grading will be done based on more
  * testcases).
  */
object Assignment1:
  /** Problem 1: Iteration of a Function.
    */

  /** Problem 1-1.
    *
    * Given a function f and integers n, x, compute f^n(x) as: if n>=1, then f^(n-1) (f(x));
    * otherwise x without using tail recursion.
    *
    * Here n is restricted by n<=50.
    */
  def iteration(f: Long => Long, n: Long, x: Long): Long = {
    if (n < 1) x
    else {
      val processed_x = iteration(f, n - 1, x)
      f(processed_x)
    }
  }

  /** Problem 1-2.
    *
    * Given a function f and integers n, x, compute f^n(x) as: if n>=1, then f^(n-1) (f(x));
    * otherwise x using tail recursion.
    */

  @tailrec
  def iterationTail(f: Long => Long, n: Long, x: Long): Long = {
    if (n < 1) x
    else iterationTail(f, n - 1, f(x))
  }

  /** Problem 2: Computing Combinations.
    *
    * Implement a function that computes the binomial coefficient.
    *
    * See https://en.wikipedia.org/wiki/Combination.
    *
    * Some test cases will have large n and i, so naive implementation may cause timeout or
    * overflow.
    *
    * Hint 1: combination(n, i) = combination(n, n-i). Hint 2: reordering multiplications and
    * divisions can prevent overflow.
    */
  @tailrec
  def combination_recursion(n: Long, i: Long, k: Long, acc: Long): Long = {
    if (k > i) acc
    else combination_recursion(n, i, k + 1, acc * (n - k + 1) / k)
  }

  def combination(n: Long, i: Long): Long = {
    if (i > n) 0L
    else if (i > n - i) combination(n, n - i)
    else combination_recursion(n, i, 1, 1)
  }

  /** Problem 3: Finding Prime Numbers.
    */

  /** Problem 3-1.
    *
    * Find out whether the given natural number is a prime number.
    */

  @tailrec
  def isPrime_recursive(p: Long, i: Long): Boolean = {
    if (i * i > p) true
    else if (p % i == 0 || p % (i + 2) == 0) false
    else isPrime_recursive(p, i + 6)
  }

  def isPrime(p: Long): Boolean = {
    if (p <= 1) false
    else if (p <= 3) true
    else if (p % 2 == 0 || p % 3 == 0) false
    else isPrime_recursive(p, 5)
  }

  /** Problem 3-2.
    *
    * Given an integer n, find the n-th twin prime.
    *
    * That is, find the n-th smallest prime number p such that p+2 is also a prime number.
    *
    * For example, the 1st twin prime is 3 (since 5 is also a prime number) and the 5th twin prime
    * is 29 (since 31 is also a prime number).
    *
    * See the website below for more information.
    *   - https://en.wikipedia.org/wiki/Twin_prime
    */

  def isTwinPrime(p: Long): Boolean = { isPrime(p) && isPrime(p + 2) }

  @tailrec
  def nthTwinPrime_recursive(n: Long, count: Long, cur_num: Long): Long = {
    if (isTwinPrime(cur_num)) {
      if (count + 1 == n) cur_num
      else nthTwinPrime_recursive(n, count + 1, cur_num + 6)
    } else {
      nthTwinPrime_recursive(n, count, cur_num + 6)
    }
  }

  def nthTwinPrime(n: Long): Long = {
    if (n == 1) 3L
    else nthTwinPrime_recursive(n, 1, 5)
  }

  /** Problem 3-3.
    *
    * Given an integer n, find two prime numbers whose sum is n. If such numbers exist, return a
    * pair of prime numbers (a, b) with the smallest a. If no such numbers exist, throw an error:
    * NoExistException(n)
    *
    * It is known that such prime numbers exist for all even number n where 2 < n <= 4 * 10^18.
    *
    * For example, 14 = 3 + 11, 58 = 11 + 37, and 158 = 31 + 127.
    *
    * See the website below for more information.
    *   - https://en.wikipedia.org/wiki/Goldbach%27s_conjecture
    */
  class NoExistException(val arg: Long) extends Exception

  def primePair_recursive(n: Long, cur_num: Long): (Long, Long) = {
    if (cur_num > n - cur_num) throw new NoExistException(n)
    else {
      if (isPrime(cur_num) && isPrime(n - cur_num)) (cur_num, n - cur_num)
      else primePair_recursive(n, cur_num + 1)
    }
  }

  def primePair(n: Long): (Long, Long) = primePair_recursive(n, 2)
