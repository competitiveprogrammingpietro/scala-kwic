import scala.collection.mutable.{Map, LinkedHashMap,ListBuffer}
import scala.collection.immutable.StringOps
import scala.util.control.Exception._

import java.io.FileWriter

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

    // Transform every line in a pair of (line, content) and filter out
    // the line not containing the specific keyword
    val filtered     = this.lines.zipWithIndex.filter(_._1.contains(keyword))
    val list         = ListBuffer[Record]()
    val unwantedChar = """[0-9]|\.|;|,|"|:""".r

    for (line <- filtered) {
      // Strip all the undesired punctuation and digits
      var transformedLine = unwantedChar.replaceAllIn(line._1.toLowerCase, "")
      println("Line "  + transformedLine)
      val keywordIndex = transformedLine.indexOf(keyword) - 1
      println("Index " + keywordIndex)
      var stringLeft = transformedLine.substring(0, scala.math.max(0, keywordIndex))
      println("Left two " + stringLeft)
      if (stringLeft.length > 30)
        stringLeft =  stringLeft.substring(stringLeft.length - 30 , 30)
      var stringRight = transformedLine.substring(keywordIndex + keyword.length + 1, transformedLine.length)
      if (stringRight.length > 30)
        stringRight = stringRight.substring(0, scala.math.min(stringRight.length, 29))

      println("String left " + stringLeft)
      println("String right " + stringRight)
      println("String keyword " + keyword)
      // Left and right justification
      val justifiedLeft  = " " * (30 - stringLeft.length)
      val justifiedRight = " " * (30 - stringRight.length)
      transformedLine = justifiedLeft + stringLeft + " " + keyword + stringRight + justifiedRight
      list += new Record(line._2 + 1, transformedLine)
    }
    this.kwicMap.+=(keyword -> list)
    list
  }

  // Write to the output file following the project's requirements
  // TODO: avoid loops, alphabetical order
  def formatOutput() : String = {
    var ret : String = this.fileName + "\n"
    for ((k,v) <- this.kwicMap) {
      v.foreach( (item) => {
        ret += item.rindex + " "
        ret += item.rcontent + "\n"
      })
    }
    ret
  }

  def writeToFile(output : FileWriter) : Unit = {
    output.write(this.formatOutput)
  }

  override def toString(): String = {
    lines.mkString("\n") +
    kwicMap.values.map(_.toString).mkString("\n") +
    this.formatOutput
  }
}

class AllDoneException extends java.lang.Exception 

object KWICFile {

  val digitRE       = """[0-9]""".r
  val punctuationRE = """.|;|,|"|:""".r 

  /* Transform the string following the project's requirement :
   * 1) Make a copy of it
   * 2) Lowercase
   * 3) Remove digits and punctuation except apostrophes
   */
}

object KWICUserInteraction {

  def main(args : Array[String]) : Unit =  {
    
    try {
      val outputFilename               = "output.txt"
      val fileStream                   = scala.io.Source.fromFile("./stop-words.txt")
      val stopWords : Array[String]    = fileStream.getLines.toArray

      fileStream.close()
      scala.util.Sorting.quickSort(stopWords)

      println ("File stop-words.txt loaded successfully : " +
        stopWords.length +
        " words found")
      val fileWriter = new FileWriter(outputFilename, true)

      // Read files' path from the standard input
      while (true) {
        val path = readLine()
        println("Processing file " + path)
        if (path.length == 0)
          throw new AllDoneException()
        try {
          val kwicFile = new KWICFile(path)
          for (keyword <- stopWords) {
            println("Examining keyword " + keyword)
            kwicFile.keywordInContext(keyword)
          }
          kwicFile.writeToFile(fileWriter)
          println(kwicFile.formatOutput)
          fileWriter.flush()
        } catch {
          case ex: java.io.FileNotFoundException => {
            ex.printStackTrace()
            println("""The mandatory file "stop-words.txt" is not present in the current directory. Aborting""")
            java.lang.System.exit(1)
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
KWICUserInteraction.main(null)
