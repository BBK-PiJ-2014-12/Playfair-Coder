/**
 * @author Zsolt Balvanyos for PPL cw Scala.
 */

import scala.io.Source
import scala.io.Codec

object Playfair extends App{
  var option: Int = 0
  
  println("Welcome to the Playfair Cipher machine!")
  println()
  
  while(option != 3){
    println("1. Encode")
    println("2. Decode")
    println("3. Quit")
    option = readLine("Please select from the above options (1 to 3): ").toInt
    
    if(option != 3) {
      val keyword = readLine("Enter keyword: ")
      val fileName = readLine("Enter filename: ")

      option match {
        case 1 =>  display(Coder(keyword).encode(getInputText(fileName)))
        case 2 =>  display(Coder(keyword).decode(getInputText(fileName)))
      }
    }
  }
  
  println("Good bye!")
  
  /**
   * Receives an encoded or a decoded string and displays it by 5 character blocks, 10 block in a row.
   * 
   * @param input string.
   */
  def display(input: String){
    var text = input
    val block = 5
    val row = 10
    var numberOfBlocks = 0
  
    while(text.length() > block) {
      print(text.substring(0, block) + " ")
      text = text.substring(block)
      numberOfBlocks = numberOfBlocks + 1
      
      if(numberOfBlocks == 10) {println; numberOfBlocks = 0}
    }
    println(text)  
  }
  
  /**
   * Reads out the the text from the file, filters out characters that are not letters and turns every letter to lower case.
   * 
   * @param name of the file.
   * @return filtered text in lower case.
   */
  def getInputText(fileName: String): String = {
    val strbld = new StringBuilder
    for (text <-  Source.fromFile(".\\src\\" + fileName, "ISO-8859-1")) strbld.+(text)
    def text(): String = strbld.filter { x => x.isLetter }.toString().toLowerCase()
    text
  }
}

