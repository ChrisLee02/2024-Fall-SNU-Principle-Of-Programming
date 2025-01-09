package pp202402.assign3b

import scala.reflect.ClassTag
import pp202402.assign3b.Data._
import pp202402.assign3b.ViewController._
import scala.annotation.tailrec

object Assignment3B:
  /** Principles of Programming: Assignment 3B. This assignment is about
    * implementing a simple game engine that can be used to play two games: Omok
    * and Sudoku.
    *
    * The game engine is implemented using the State design pattern, where the
    * state of the game is represented by `State`.
    *   - `State`: contains the current state of the game board and the current
    *     player. The game engine also uses the Rule design pattern, where the
    *     rules of the game are implemented in `Rule`.
    *   - `Rule`: contains the rules of the game, such as the initial message,
    *     initial value, and the rules for making a move. The game engine also
    *     uses the ViewController design pattern, where the view of the game is
    *     implemented in a `ViewController`.
    *   - `ViewController`: contains the methods to display the game board and
    *     the values on the board. The game engine also uses the GameController
    *     class to control the game.
    *   - `GameController`: contains the methods to initialize the game, display
    *     the game board, and make a move.
    *
    * The game engine is implemented in a generic way, so it can be used to play
    * any game that can be represented by the `State`, `Rule`, and
    * `ViewController` classes. Check `UI.scala` to understand how the game
    * engine is used to play Omok and Sudoku.
    *
    * Fill in the TODOs
    *   - in the `OmokState` and `SudokuState` classes to implement the state of
    *     the Omok and Sudoku games.
    *   - in the `OmokRule` and `SudokuRule` classes to implement the rules of
    *     the Omok and Sudoku games.
    *   - in the `GameController` class to implement the game controller.
    *
    * You are free to implement more functions inside the classes if needed.
    *
    * There are no TODOs on other files, but you can check them to understand
    * the game engine. We will only extract your `Main.scala` to grade your
    * assignment. Thus, please do not make changes on other files.
    */

  /** Omok game.
    *
    * Omok is a two-player game where the players take turns to fill the board
    * with Os and Xs.
    *   - The game board is a 5x5 board.
    *   - A player wins if they have 5 consecutive pieces in a row, column, or
    *     diagonal.
    *   - Player 1 ALWAYS makes the first move when the game starts.
    *   - Also, players will take turns to fill the board until one player wins
    *     or the board is full without a winner.
    */

  /** Omok state. Here, you should implement the state of the Omok game using
    * the `OmokState` class that extends the `State` class. There are two states
    * in the Omok game:
    *   - a board with "O"s, "X"s, and empty cells (" "). Each cell is
    *     represented by a string value.
    *   - the current player.
    *
    * Also, there are two methods in the `OmokState` class:
    *   - `fillCell`: fills the cell at the given row and column with the given
    *     value.
    *   - `nextTurn`: returns the state that represents the next turn. If the
    *     current player is Player1, the next player is Player2. If the current
    *     player is Player2, the next player is Player1. Here, note that we have
    *     splitted the stage of updating the board and the stage of updating the
    *     player, but a consistent game state requires the all the updates to be
    *     done at one turn.
    */
  class OmokState(board: MyArray[String], currentPlayer: Player)
      extends State[String](board, currentPlayer, 5, 5):
    // TODOs start here //
    def fillCell(i: Int, j: Int, v: String): OmokState = {
      new OmokState(board.update(i * 5 + j, v), currentPlayer)
    }

    def nextTurn(): OmokState = {
      if (currentPlayer == Player.Player1) {
        new OmokState(board, Player.Player2)
      } else {
        new OmokState(board, Player.Player1)
      }
    }

  /** Omok rule.
    *
    * Here, you should implement the rules of the Omok game using the `OmokRule`
    * class that extends the `Rule` class. We will introduce rules that we are
    * going to check in public/hidden tests. We will not check the rules that
    * are not introduced here.
    */
  class OmokRule extends Rule[String]:
    def initMessage(): String =
      "Omok: Fill the board with Os and Xs. A player wins if they have 5 consecutive pieces in a row, column, or diagonal.\n" +
        "Player 1 is O and Player 2 is X. Player 1 starts first.\n"

    def initValue(): String = " "
    def player1: String = "O"
    def player2: String = "X"

    // TODOs start here //

    // TODO: Implement the renderInput method for the Omok game.
    // Render a input character into a string value. A valid input should be either 'O' or 'X'.
    // If the input is not valid, throw an InvalidInputException.
    def renderInput(input: Char): String = {
      if (input == 'O') {
        "O"
      } else if (input == 'X') {
        "X"
      } else throw InvalidInputException("Invalid Input")
    }

    // TODO: Implement the isAvailableValue method for the Omok game.
    // - check1 if the value is valid. A valid value should be either "O" or "X".
    def isAvailableValue(v: String): Boolean = {
      v == "O" || v == "X"
    }

    // TODO: Implement the isStateValid method for the Omok game.
    // Given a current state with a board and the current player,
    // check if the state is consistent
    // - hint: think about how many pieces each player can have on the board when they take turns.
    def isStateValid(state: State[String]): Boolean = {
      def checkValidity(idx: Int): Boolean =
        if (idx >= 25) true
        else {
          val cell = state.getCell(idx / 5, idx % 5)
          if (cell == "O" || cell == "X" || cell == " ") checkValidity(idx + 1)
          else false
        }

      def countPieces(idx: Int, countO: Int, countX: Int): (Int, Int) =
        if (idx >= 25) (countO, countX)
        else {
          val cell = state.getCell(idx / 5, idx % 5)
          if (cell == "O") countPieces(idx + 1, countO + 1, countX)
          else if (cell == "X") countPieces(idx + 1, countO, countX + 1)
          else countPieces(idx + 1, countO, countX)
        }

      if (!checkValidity(0)) false
      else {
        val (countO, countX) = countPieces(0, 0, 0)
        state.getPlayer() match {
          case Player.Player1 => countO == countX
          case Player.Player2 => countO == countX + 1
        }
      }

    }

    // TODO: Implement the isNextMoveValid method for the Omok game.
    // Given a current state with a board, the current player,
    // and the next move (i, j, v) this player wants to make,
    // check if the next move is valid.
    // - check1: you cannot put a piece on a cell that is already filled.
    // - check2: you cannot put a piece on a cell if the game is already done.
    def isNextMoveValid(
        state: State[String],
        i: Int,
        j: Int,
        v: String
    ): Boolean = {
      def isMatchedValue(): Boolean = {
        (v == "O" && state.getPlayer() == Player.Player1)
        || (v == "X" && state.getPlayer() == Player.Player2)
      }
      isMatchedValue() && !isDone(state) && state.getCell(i, j) == " "
    }

    // TODO: Implement the isDone method for the Omok game.
    // Given a current state with a board, check if the game is done.
    // The game is done if one player wins or the board is full.

    def isDone(state: State[String]): Boolean = {
      def isFull(idx: Int): Boolean =
        if (idx >= 25) true
        else if (state.getCell(idx / 5, idx % 5) == " ") false
        else isFull(idx + 1)

      def checkRows(row: Int): Boolean =
        if (row >= 5) false
        else {
          def checkRow(col: Int, prev: String): Boolean =
            if (col >= 5) prev != " "
            else {
              val cell = state.getCell(row, col)
              if (cell != prev) false
              else checkRow(col + 1, cell)
            }

          if (checkRow(1, state.getCell(row, 0))) true
          else checkRows(row + 1)
        }

      def checkColumns(col: Int): Boolean =
        if (col >= 5) false
        else {
          def checkColumn(row: Int, prev: String): Boolean =
            if (row >= 5) prev != " "
            else {
              val cell = state.getCell(row, col)
              if (cell != prev) false
              else checkColumn(row + 1, cell)
            }

          if (checkColumn(1, state.getCell(0, col))) true
          else checkColumns(col + 1)
        }

      def checkDiagonals(): Boolean = {
        def checkDiag1(idx: Int, prev: String): Boolean =
          if (idx >= 5) prev != " "
          else {
            val cell = state.getCell(idx, idx)
            if (cell != prev) false
            else checkDiag1(idx + 1, cell)
          }

        def checkDiag2(idx: Int, prev: String): Boolean =
          if (idx >= 5) prev != " "
          else {
            val cell = state.getCell(idx, 4 - idx)
            if (cell != prev) false
            else checkDiag2(idx + 1, cell)
          }

        checkDiag1(1, state.getCell(0, 0)) || checkDiag2(1, state.getCell(0, 4))
      }

      isFull(0) || checkRows(0) || checkColumns(0) || checkDiagonals()
    }

  /** Sudoku game.
    *
    * Sudoku is a single-player game where the player fills the board with
    * numbers from 1 to 9.
    *   - The game board is a 9x9 board.
    *   - No number can be repeated in the same row, column, or 3x3 subgrid.
    *   - The game is done when the board is full and the rules are satisfied.
    *   - The player do not have to check if the initial board is can be solved
    *     or not.
    */

  /** Sudoku state. Here, you should implement the state of the Sudoku game
    * using the `SudokuState` class that extends the `State` class. There are
    * two states in the Sudoku game:
    *   - a board with numbers from 1 to 9 and empty cells. Each cell is
    *     represented by a `SudokuCell` value.
    *   - the current player.
    *
    * Also, there are two methods in the `SudokuState` class:
    *   - `fillCell`: fills the cell at the given row and column with the given
    *     value.
    *   - `nextTurn`: returns the state that represents the next turn.
    */
  class SudokuState(board: MyArray[SudokuCell], currentPlayer: Player)
      extends State[SudokuCell](board, currentPlayer, 9, 9):

    def fillCell(i: Int, j: Int, v: SudokuCell): SudokuState = {
      new SudokuState(board.update(i * 9 + j, v), currentPlayer)
    }

    def nextTurn(): SudokuState = this

  /** Sudoku rule.
    *
    * Here, you should implement the rules of the Sudoku game using the
    * `SudokuRule` class that extends the `Rule` class. We will introduce rules
    * that we are going to check in public/hidden tests. We will not check the
    * rules that are not introduced here.
    */
  class SudokuRule extends Rule[SudokuCell]:
    def initMessage(): String =
      "Sudoku: Fill the board with numbers from 1 to 9.\n" +
        "No number can be repeated in the same row, column, or 3x3 subgrid.\n"
    def initValue(): SudokuCell = SudokuCell.Empty

    // TODOs start here

    // TODO: Implement the renderInput method for the Sudoku game.
    // Render a input character into a SudokuCell value. A valid input should be a character from '1' to '9'.
    // If the input is not valid, throw an InvalidInputException.
    // You can assume that only player input will be passed to this method. (No fixed values)
    def renderInput(input: Char): SudokuCell = {
      if (input >= '1' && input <= '9') {
        SudokuCell.UserValue(input.asDigit)
      } else throw InvalidInputException("Invalid Input")
    }

    // TODO: Implement the isAvailableValue method for the Sudoku game.
    // Determine if the input SudokuCell is available to put on the board.
    // - check1: a player is allowed to put a number from 1 to 9.
    // - check2: a player is allowed to put an empty cell.
    def isAvailableValue(v: SudokuCell): Boolean = {
      v match
        case SudokuCell.Empty         => true
        case SudokuCell.UserValue(v)  => v >= 1 && v <= 9
        case SudokuCell.FixedValue(_) => false
    }

    // TODO: Implement the isStateValid method for the Sudoku game.
    // Given a current state with a board and the current player,
    // check if the state is consistent
    // - check1: No number can be repeated in the same row, column, or 3x3 subgrid.
    def isStateValid(state: State[SudokuCell]): Boolean = {
      val N = 9
      val N_sqrt = 3

      def countOccurrences(
          rowStart: Int,
          colStart: Int,
          rowEnd: Int,
          colEnd: Int,
          num: Int
      ): Int = {
        def count(row: Int, col: Int, acc: Int): Int = {
          if (row > rowEnd) acc
          else if (col > colEnd) count(row + 1, colStart, acc)
          else
            state.getCell(row, col) match {
              case SudokuCell.UserValue(v) if v == num =>
                count(row, col + 1, acc + 1)
              case SudokuCell.FixedValue(v) if v == num =>
                count(row, col + 1, acc + 1)
              case _ => count(row, col + 1, acc)
            }
        }
        count(rowStart, colStart, 0)
      }

      def isRangeValid(
          rowStart: Int,
          colStart: Int,
          rowEnd: Int,
          colEnd: Int
      ): Boolean = {
        def checkNum(num: Int): Boolean = {
          if (num > N) true
          else if (
            countOccurrences(rowStart, colStart, rowEnd, colEnd, num) > 1
          ) false
          else checkNum(num + 1)
        }
        checkNum(1)
      }

      def checkRows(row: Int): Boolean = {
        if (row >= N) true
        else if (!isRangeValid(row, 0, row, N - 1)) false
        else checkRows(row + 1)
      }

      def checkColumns(col: Int): Boolean = {
        if (col >= N) true
        else if (!isRangeValid(0, col, N - 1, col)) false
        else checkColumns(col + 1)
      }

      def checkSubGrids(rowBase: Int, colBase: Int): Boolean = {
        if (rowBase >= N) true
        else if (colBase >= N) checkSubGrids(rowBase + N_sqrt, 0)
        else if (
          !isRangeValid(
            rowBase,
            colBase,
            rowBase + N_sqrt - 1,
            colBase + N_sqrt - 1
          )
        ) false
        else checkSubGrids(rowBase, colBase + N_sqrt)
      }

      checkRows(0) && checkColumns(0) && checkSubGrids(0, 0)
    }

    // TODO: Implement the isNextMoveValid method for the Sudoku game.
    // Given a current state with a board, the current player,
    // and the next move (i, j, v) this player wants to make,
    // check if the next move is valid.
    // - check1: you cannot put a number on a cell that is already fixed.
    // - check2: you cannot put a number on a cell if the game is already done.
    def isNextMoveValid(
        state: State[SudokuCell],
        i: Int,
        j: Int,
        v: SudokuCell
    ): Boolean = {
      if (isDone(state)) false
      else {
        val cell = state.getCell(i, j)
        cell match
          case SudokuCell.FixedValue(v) => false
          case _ => {
            val new_state = state.fillCell(i, j, v)
            isStateValid(new_state)
          }
      }
    }

    // TODO: Implement the isDone method for the Sudoku game.
    // Given a current state with a board, check if the game is done.
    // - check1: The game is done if the board is full and the rules are satisfied.
    def isDone(state: State[SudokuCell]): Boolean = {
      def isFull(state: State[SudokuCell]): Boolean = {
        def checkFull(row: Int, col: Int): Boolean = {
          if (row >= 9) true
          else if (col >= 9) checkFull(row + 1, 0)
          else
            state.getCell(row, col) match {
              case SudokuCell.Empty => false
              case _                => checkFull(row, col + 1)
            }
        }
        checkFull(0, 0)
      }
      isFull(state) && isStateValid(state)
    }

  /** Game controller. Implement the game controller using the `GameController`
    * class.
    */
  class GameController[A: ClassTag](
      rule: Rule[A],
      state: State[A],
      viewController: ViewController[A]
  ):
    def init(): String = rule.initMessage()

    def display(): String = viewController.displayBoardState(state)

    def isDone(): Boolean = rule.isDone(state)

    // TODO starts here //

    // TODO: Implement the makeMove method for the game controller.
    // Given the row, column, and value (Char) the player wants to put on the board,
    // check if the move is valid and update the game state.
    // If the move is not valid, throw an InvalidMoveException.
    def makeMove(i: Int, j: Int, v: Char): GameController[A] = {
      val value = rule.renderInput(v)
      if (
        !rule.isAvailableValue(value)
        || !rule
          .isStateValid(state)
        || !rule.isNextMoveValid(state, i, j, value)
      )
        throw InvalidMoveException("Invalid Move")

      val new_state = state.fillCell(i, j, value).nextTurn()
      new GameController(rule, new_state, viewController)
    }
