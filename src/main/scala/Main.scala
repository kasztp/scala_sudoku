@main def hello: Unit =
  val filename = scala.io.StdIn.readLine("Enter path to input file: ")
  if filename == "" then
    println("No input given, exiting!")
    return
  println(s"Reading file from: $filename")
  var sudoku_challenge = read_sudoku(filename)
  print("Input:")
  print_sudoku(sudoku_challenge)
  val start_time = System.currentTimeMillis()
  if solve_sudoku(sudoku_challenge) then
    val end_time = System.currentTimeMillis()
    val time_taken = end_time - start_time
    print("Solution:")
    print_sudoku(sudoku_challenge)
    println(s"Time taken: ${time_taken}ms")
  else
    println("No solution found!")

def sudoku_line = "-------------"


def read_sudoku(filename: String): Array[Array[Int]] = {
  val lines = scala.io.Source.fromFile(filename).getLines.toArray
  val sudoku = Array.ofDim[Int](9, 9)
  for (i <- 0 until 9) {
    val line = lines(i).split(",")
    for (j <- 0 until 9) {
      sudoku(i)(j) = line(j).toInt
    }
  }
  sudoku
}


def print_sudoku(sudoku: Array[Array[Int]]): Unit = {
  println()
  println(sudoku_line)
  for (i <- 0 until 9) {
    print("|")
    for (j <- 0 until 9) {
      print(sudoku(i)(j))
      if (j % 3 == 2) {
        print("|")
      }
    }
    println()
    if (i % 3 == 2) {
      println(sudoku_line)
    }
  }
}


def solve_sudoku(sudoku: Array[Array[Int]]): Boolean = {
  val empty_cell = find_empty_cell(sudoku)
  if (empty_cell == null) {
    return true
  }
  val row = empty_cell(0)
  val col = empty_cell(1)
  for (num <- 1 to 9) {
    if (is_valid(sudoku, row, col, num)) {
      sudoku(row)(col) = num
      if (solve_sudoku(sudoku)) {
        return true
      }
      sudoku(row)(col) = 0
    }
  }
  false
}


def find_empty_cell(sudoku: Array[Array[Int]]): Array[Int] = {
  for (i <- 0 until 9) {
    for (j <- 0 until 9) {
      if (sudoku(i)(j) == 0) {
        return Array(i, j)
      }
    }
  }
  null
}


def is_valid(sudoku: Array[Array[Int]], row: Int, col: Int, num: Int): Boolean = {
  for (i <- 0 until 9) {
    if (sudoku(row)(i) == num) {
      return false
    }
  }
  for (i <- 0 until 9) {
    if (sudoku(i)(col) == num) {
      return false
    }
  }
  val box_row = row - row % 3
  val box_col = col - col % 3
  for (i <- 0 until 3) {
    for (j <- 0 until 3) {
      if (sudoku(box_row + i)(box_col + j) == num) {
        return false
      }
    }
  }
  true
}
