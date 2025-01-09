import scala.annotation.tailrec

import pp202402.assign4a.Assignment4A.{fibo, getN, toList}
import pp202402.assign4a.{Data, Lazylist}

class TestSuite extends munit.FunSuite {
  import Data.*
  import Lazylist.*

  test("problem 1") {
    assertEquals(toList(getN(fibo[Lazylist], 6)), List(0L, 1L, 1L, 2L, 3L, 5L))
  }
  test("problem 2") {
    assertEquals(
      toList(getN(fibo[Lazylist], 10)),
      List(0L, 1L, 1L, 2L, 3L, 5L, 8L, 13L, 21L, 34L)
    )
  }

}
