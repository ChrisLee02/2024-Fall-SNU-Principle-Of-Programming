package pp202402.assign2b

import scala.annotation.tailrec
import scala.util.control.TailCalls._

/** Principles of Programming: Assignment 2B.
  *
  * Implement given functions, which are currently left blank. (???) **WARNING: Please read the
  * restrictions below carefully.**
  *
  * If you do not follow these, **your submission will not be graded.**
  *
  *   - Do not use the keyword `var`. Use `val` and `def` instead.
  *   - Do not use any library functions or data structures like `List`, `Range` (`1 to n`, `1 until
  *     n` ...), `fold`, `map`, `reduce` or etc except for `Array`.
  *   - If you want to use a data structure, create new one instead of using the library ones.
  *   - You can only use tuples, `scala.annotation.tailrec`, and `scala.util.control.TailCalls._`,
  *     `Array` from the library.
  *   - Do not use any looping syntax of Scala (`for`, `while`, `do-while`, `yield`, ...)
  *
  * Again, your score will be zero if you do not follow these rules.
  *
  * Note that these rules will be gradually relaxed through the next assignments.
  *
  * We do not require tail-recursion explicitly for this assignment.
  *
  * Timeout: 30 sec.
  */
object Assignment2B:
  import IOption.*

  /** Problem 3: Sudoku Solver
    *
    * Solve a given 9x9 Sudoku puzzle. A Sudoku board is a 9x9 grid, where some cells are pre-filled
    * with numbers between 1 and 9, and other cells are empty (represented by 0). The goal is to
    * fill all the empty cells with numbers between 1 and 9 while satisfying the following
    * conditions:
    *
    *   1. Each number from 1 to 9 must appear exactly once in each row. 2. Each number from 1 to 9
    *      must appear exactly once in each column. 3. Each number from 1 to 9 must appear exactly
    *      once in each of the nine 3x3 subgrids.
    *
    * This means that for any empty cell, you need to find a valid number such that it does not
    * already exist in its corresponding row, column, or subgrid. Check the link below for more
    * information on Sudoku puzzles: https://en.wikipedia.org/wiki/Sudoku
    *
    * The function `solveSudoku(board: Array[Int]): IOption[Array[Int]]` takes a 9x9 Sudoku board as
    * input and returns an `IOption` of the solved board. If the board is solvable, the function
    * should return `ISome(solvedBoard)`. If the board is not solvable, the function should return
    * `INone`.
    *
    * Note: While you are not allowed to use `Array` in other problems, we have provided a board as
    * a 1D array using `Array` here. You can use indexing to access elements in the board. You can
    * think of this 1D array as a flattened version of a 2D array, where each row is concatenated
    * sequentially. You can access to the value of an array with the following syntax: `A(3)` for
    * the 3rd element of array A.
    */
  def solveSudoku(board: Array[Int]): IOption[Array[Int]] = {
    val N = 9
    val N_sqrt = 3
    def convertIndex(row: Int, col: Int): Int = row * N + col
    def getElement(board: Array[Int], row: Int, col: Int): Int =
      board(convertIndex(row, col))
    def updateElement(board: Array[Int], row: Int, col: Int, num: Int): Unit =
      board(convertIndex(row, col)) = num

    def checkRow(
        board: Array[Int],
        row: Int,
        num: Int,
        col_pos: Int
    ): Boolean = {
      if (col_pos == N) true
      else if (getElement(board, row, col_pos) == num) false
      else checkRow(board, row, num, col_pos + 1)
    }

    def checkCol(
        board: Array[Int],
        col: Int,
        num: Int,
        row_pos: Int
    ): Boolean = {
      if (row_pos == N) true
      else if (getElement(board, row_pos, col) == num) false
      else checkCol(board, col, num, row_pos + 1)
    }

    def checkSubBoard(
        board: Array[Int],
        row_base: Int,
        col_base: Int,
        row_offset: Int,
        col_offset: Int,
        num: Int
    ): Boolean = {
      if (row_offset == N_sqrt) true
      else if (getElement(board, row_base + row_offset, col_base + col_offset) == num) false
      else {
        val (row_offset_next, col_offset_next) =
          if (col_offset == N_sqrt - 1) (row_offset + 1, 0)
          else (row_offset, col_offset + 1)
        checkSubBoard(
          board,
          // 0,1,2 -> 0 3,4,5 -> 3, 6,7,8 -> 6
          row_base,
          col_base,
          row_offset_next,
          col_offset_next,
          num
        )
      }
    }

    def checkValidity(
        board: Array[Int],
        row: Int,
        col: Int,
        num: Int
    ): Boolean = {
      checkRow(board, row, num, 0) && checkCol(
        board,
        col,
        num,
        0
      ) && checkSubBoard(
        board,
        // 0,1,2 -> 0 3,4,5 -> 3, 6,7,8 -> 6
        (row / N_sqrt) * N_sqrt,
        (col / N_sqrt) * N_sqrt,
        0,
        0,
        num
      )
    }

    def finalCheck(board: Array[Int], row: Int, col: Int): Boolean = {
      if (row == N) true
      else if (col == N) finalCheck(board, row + 1, 0)
      else {
        val num = getElement(board, row, col)
        if (num != 0) {
          updateElement(board, row, col, 0)
          val valid = checkValidity(board, row, col, num)
          updateElement(board, row, col, num)
          if (!valid) false
          else finalCheck(board, row, col + 1)
        } else {
          finalCheck(board, row, col + 1)
        }
      }
    }

    def try_one_to_nine(
        board: Array[Int],
        row: Int,
        col: Int,
        num: Int
    ): IOption[Array[Int]] = {
      if (num == N + 1) IOption.INone
      else if (checkValidity(board, row, col, num)) {
        val (next_row, next_col) =
          if (col == N - 1) (row + 1, 0) else (row, col + 1)

        updateElement(board, row, col, num)
        backtrack(board, next_row, next_col) match
          case ISome(a) => ISome(a)
          case INone => {
            updateElement(board, row, col, 0)
            try_one_to_nine(board, row, col, num + 1)
          }

      } else {
        try_one_to_nine(board, row, col, num + 1)
      }
    }

    def backtrack(
        board: Array[Int],
        row: Int,
        col: Int
    ): IOption[Array[Int]] = {
      if (row == N) {
        if (finalCheck(board, 0, 0)) IOption.ISome(board)
        else IOption.INone
      } else {
        val (next_row, next_col) =
          if (col == N - 1) (row + 1, 0) else (row, col + 1)

        if (getElement(board, row, col) != 0)
          backtrack(board, next_row, next_col)
        else try_one_to_nine(board, row, col, 1)
      }

    }

    backtrack(board, 0, 0)
  }
