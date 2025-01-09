// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import scala.annotation.tailrec
import scala.math._

import pp202402.assign3a.Assignment3A.{decSqrt}
import pp202402.assign3a.{Data}

class TestSuite extends munit.FunSuite {
  import Data.*

  def zeros(n: Int): List[Int] = if n <= 0 then Nil else 0 :: zeros(n - 1)
  def getN[A](a: LazyList[Int], n: Int): List[Int] =
    if (n <= 0) Nil
    else {
      a.get match
        case None       => zeros(n)
        case Some(h, t) => h :: getN(t, n - 1)
    }

  test("problem 1") {
    assertEquals(
      getN(decSqrt(LCons(2, LNil()), 5), 7),
      List(1, 4, 1, 4, 2, 1, 0)
    )
    assertEquals(
      getN(decSqrt(LCons(0, LCons(7, LNil())), 5), 7),
      List(0, 8, 3, 6, 6, 6, 0)
    )
    assertEquals(
      getN(decSqrt(LCons(1, LCons(4, LCons(4, LNil()))), 5), 7),
      List(1, 2, 0, 0, 0, 0, 0)
    )
  }
  test("problem 2") {
    assertEquals(
      getN(decSqrt(LCons(2, LNil()), 10), 2),
      List(1, 4)
    )
  }

  test("problem 3: edge case sqrt(0)") {
    assertEquals(
      getN(decSqrt(LCons(0, LNil()), 5), 7),
      List(0, 0, 0, 0, 0, 0, 0)
    )
  }

  test("problem 4: edge case sqrt(1)") {
    assertEquals(
      getN(decSqrt(LCons(1, LNil()), 5), 7),
      List(1, 0, 0, 0, 0, 0, 0)
    )
  }

  test("problem 5: sqrt(9.87654321)") {
    val input = LCons(
      9,
      LCons(
        8,
        LCons(
          7,
          LCons(6, LCons(5, LCons(4, LCons(3, LCons(2, LCons(1, LNil()))))))
        )
      )
    )
    assertEquals(
      getN(decSqrt(input, 8), 11),
      List(3, 1, 4, 2, 6, 9, 6, 8, 0, 0, 0)
    )
  }

  test("problem 6: sqrt(0.01)") {
    val input = LCons(0, LCons(0, LCons(1, LNil())))
    assertEquals(
      getN(decSqrt(input, 5), 7),
      List(0, 1, 0, 0, 0, 0, 0)
    )
  }
  test("problem 7: sqrt(0.1)") {
    val input = LCons(0, LCons(1, LNil()))
    assertEquals(
      getN(decSqrt(input, 20), 12),
      List(0, 3, 1, 6, 2, 2, 7, 7, 6, 6, 0, 1)
    )
  }

}
