import java.util.Collections

/**
 * Receives a keyword and creates a 5*5 matrix with it.
 */
case class CodeBlock(keyword: String){
  
  val filteredKey = keyword.toLowerCase().distinct
  val abc = new StringBuilder()
  for(n <- 'a' to 'z') abc.+(n)
  val full = filteredKey + abc
  val code = full.distinct filter (n => n != 'j')
  
  val lineOne = Array(code.charAt(0), code.charAt(1), code.charAt(2), code.charAt(3), code.charAt(4))
  val lineTwo = Array(code.charAt(5), code.charAt(6), code.charAt(7), code.charAt(8), code.charAt(9))
  val lineThree = Array(code.charAt(10), code.charAt(11), code.charAt(12), code.charAt(13), code.charAt(14))
  val lineFour = Array(code.charAt(15), code.charAt(16), code.charAt(17), code.charAt(18), code.charAt(19))
  val lineFive = Array(code.charAt(20), code.charAt(21), code.charAt(22), code.charAt(23), code.charAt(24))
  
  val matrix = Array(lineOne, lineTwo, lineThree, lineFour, lineFive)
  
  /**
   * Prints the matrix.
   */
  def printMatrix() = {
    for(i <- 0 to 4) {
      for(j <- 0 to 4) {
        print(matrix(i)(j))
      }
      println()
    }
  }    
}