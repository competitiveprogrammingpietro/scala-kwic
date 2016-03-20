import scala.collection.mutable.{Map, LinkedHashMap,ListBuffer}
import scala.collection.immutable.StringOps
import scala.util.control.Exception._

import java.io.FileWriter

// Pietro Paolini - Birkbeck University
// Programming Paradigms 2015/2016 Scala programming coursework

class Record(line: Int, content: String) {
  val rindex   = line
  val rcontent = content

  override def toString() : String = "Line:" + rindex + " Content: " + rcontent
}

class KWICFile(filename: String) {
  
  val fileName  = filename
  val fileSource = scala.io.Source.fromFile(filename)
  val lines     = fileSource.getLines.toArray
  val kwicMap   = new LinkedHashMap[String, ListBuffer[Record]]


  fileSource.close()
  
  def keywordInContext(keyword: String) : ListBuffer[Record] =  {

    /* 
     * Transform every line in a pair of (line, content) then filter out
     * all lines not containing the specific keyword
     */
    val filtered     = this.lines.zipWithIndex.filter(_._1.contains(keyword))
    val list         = ListBuffer[Record]()
    val unwantedChar = """[0-9]|\.|;|,|"|:""".r

    for (line <- filtered) {

      /* Transform the string following the project's requirement :
       * 1) Make a copy of it
       * 2) Lowercase
       * 3) Remove digits and punctuations except apostrophes
       * 4) Add text justification
       */
      var transformedLine = unwantedChar.replaceAllIn(line._1.toLowerCase, "")
      val keywordIndex = transformedLine.indexOf(keyword) - 1
      var stringLeft = transformedLine.substring(0, scala.math.max(0, keywordIndex))

      // Cut the string at 30 chars - left
      if (stringLeft.length > 30)
        stringLeft =  stringLeft.substring(stringLeft.length - 1 - 30 , stringLeft.length - 1)
      var stringRight = transformedLine.substring(keywordIndex + keyword.length + 1, transformedLine.length)

      // Cut the string at 30 chars - right
      if (stringRight.length > 30)
        stringRight = stringRight.substring(0, scala.math.min(stringRight.length, 30))

      // Left and right justification
      val justifiedLeft  = " " * (30 - stringLeft.length)
      val justifiedRight = " " * (30 - stringRight.length)
      transformedLine = justifiedLeft + stringLeft + " " + keyword + stringRight + justifiedRight
      list += new Record(line._2 + 1, transformedLine)
    }
    this.kwicMap.+=(keyword -> list)
    list
  }

  /* 
   * Format an output string following the project's requirements
   * 1) Filename
   * 2) Keyword in alphabetical order
   */
  def formatOutput() : String = {
    var ret : String = this.fileName + "\n"
    for ((k,v) <- this.kwicMap) {

      // High order function usage
      v.foreach( (item) => {
        ret += item.rindex + " "
        ret += item.rcontent + "\n"
      })
    }
    ret
  }

  /* Writes the output to the provided file */
  def writeToFile(output : FileWriter) : Unit = {
    output.write(this.formatOutput)
  }

  override def toString(): String = {
    lines.mkString("\n") +
    kwicMap.values.map(_.toString).mkString("\n") +
    this.formatOutput
  }
}

/* Exception used to handle the case where the user entered an empty path to end
 * the program execution
 */
class AllDoneException extends java.lang.Exception 

object KWICFile {

  def main(args : Array[String]) : Unit =  {
    
    try {
      val outputFilename               = "output.txt"
      val fileStream                   = scala.io.Source.fromFile("./stop-words.txt")
      val stopWords : Array[String]    = fileStream.getLines.toArray

      fileStream.close()

      // The LinkedHashMap data structure used by the KWICFile to implement the
      // map assures keys to be iterated in the same order as they were
      // inserted, as a result of that sorting the stop words array causes the
      // Map to return an alphabetical ordered iterator
      scala.util.Sorting.quickSort(stopWords)
      println ("File stop-words.txt loaded successfully : " +
        stopWords.length +
        " words found")
      val fileWriter = new FileWriter(outputFilename, true)

      // Read files' path from the standard input
      while (true) {
        val path = readLine()
        if (path.length == 0)
          throw new AllDoneException()
        try {
          val kwicFile = new KWICFile(path)
          for (keyword <- stopWords)
            kwicFile.keywordInContext(keyword)
          kwicFile.writeToFile(fileWriter)
          fileWriter.flush()
        } catch {
          case ex: java.io.FileNotFoundException => {
            println("""File not found, please try again""")
          }
        }
      }
    } catch {
      case ex: java.io.FileNotFoundException => {
        ex.printStackTrace()
        println("""The mandatory file "stop-words.txt" is not present in the current directory. Aborting""")
        java.lang.System.exit(1)
      }
      case ex: AllDoneException => {
        println("Execution terminated")
        return
      }
    }
  }
}


//KWICUserInteraction.main(null)
