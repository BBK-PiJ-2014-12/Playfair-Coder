import  scala.io.Source,  scala.io.Source._

/**
 * @author Zsolt Balvanyos
 */

case class Coder (keyword: String) {
  val matrix = CodeBlock(keyword).matrix
  val flatMatrix = matrix.flatten
  
  /**
   * Decouples double letters and passes them to the 'transformer' method two by two.
   * 
   * @param the string to encode.
   * @return the encoded string.
   */
  def encode(plainText: String): String = {
    if(plainText.equals("")) return ""
    
    var firstChar: Character = null
    var secChar: Character = null
    val size = plainText.length()
    var i = 0
    val result = new StringBuilder
    
    while (i < size) {
      firstChar = plainText.charAt(i)
         
      if(i < size - 1 && plainText.charAt(i) == 'x' && plainText.charAt(i + 1) == 'x') {
        secChar = 'q'
        i = i + 1
      } else if(i < size - 1 && plainText.charAt(i) == plainText.charAt(i + 1)) {
        secChar = 'x'
        i = i + 1
      } else {
        if(i < plainText.length() - 1) secChar = plainText.charAt(i + 1) 
        else secChar = 'z'
        i = i + 2        
      }
      val (a, b) = transform(firstChar, secChar, 1)
      result.+(a).+(b)
    }
    return result.toString()
  }
  
  /**
   * Passes the letters two by two to the 'transformer' method and when it has built up the decoded string,
   * it passes to the 'filterDecoded' method to remove any letter that was added during encoding
   * 
   * @param the string to encode.
   * @return the encoded string.
   */
  def decode(secretText: String): String = {
    if(secretText.equals("")) return ""
    
    var firstChar: Character = null
    var secChar: Character = null
    val size = secretText.length()
    var i = 0
    val result = new StringBuilder
    
    while (i < size) {
      firstChar = secretText.charAt(i)
      secChar = secretText.charAt(i + 1)
      i = i + 2 
      val (a, b) = transform(firstChar, secChar, 4)
      result.+(a).+(b)
    }
    return filterDecoded(result.toString())
  }
  
   /**
    * Retrieves the new letters from the matrix. 
    * 
    * @param two characters to transform.
    * @return the transformed characters.
    */
  def transform(firstChar: Character, secChar: Character, offset: Int): (Character, Character) = {
      val (fx, fy) = getPosition(firstChar)
      val (sx, sy) = getPosition(secChar)
      var returnOne: Character = null
      var returnTwo: Character = null
      
      if(fx == sx) {
        returnOne = matrix(fx)((fy + offset) % 5)
        returnTwo = matrix(sx)((sy + offset) % 5)
      } else if(fy == sy) {
        returnOne = matrix((fx + offset) % 5)(fy)
        returnTwo = matrix((sx + offset) % 5)(sy)
      } else {
        returnOne = matrix(fx)(sy)
        returnTwo = matrix(sx)(fy)        
      }
      return (returnOne, returnTwo)
  }
  
  /**
   * Finds the queried character's position in the matrix. 
   * 
   * @param character to be located.
   * @return x and y describing the position of the character in the matrix.
   */
  def getPosition(ch: Character): (Int, Int) = {
      var index = 0
      for(i <- 0 to flatMatrix.length - 1) { 
        if(flatMatrix.charAt(i) == ch) {
          index = i
        }
      }
      val y = index % 5
      val x = index / 5
      (x, y)
  }
  
  /**
   * Removes letters that were added during encoding
   * 
   * @param decoded string 
   * @return the filtered decoded string
   */
  def filterDecoded(decodedText: String): String = {
    var result = ""
    var i = 0
    while(i < decodedText.length()) {
      if(i > 0 && i < decodedText.length() - 2) {
        if(decodedText.charAt(i) == 'x' && decodedText.charAt(i - 1) == decodedText.charAt(i + 1)) {i = i + 1}
        if(decodedText.charAt(i) == 'q' && decodedText.charAt(i - 1) == 'x' && decodedText.charAt(i + 1) == 'x') {i = i + 1}
      }
      result = result + decodedText.charAt(i)
      i = i + 1
    }
    result
  }
}