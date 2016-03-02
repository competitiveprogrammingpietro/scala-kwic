import scala.collection.mutable.{Map, HashMap,ListBuffer}
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
  val kwicMap   = new HashMap[String, ListBuffer[Record]]

  fileSource.close()
  
  def keywordInContext(keyword: String) : ListBuffer[Record] =  {

    // Transform every line in a pair of (line, content) and filter out
    // the line not containing the specific keyword
    val filtered = this.lines.zipWithIndex.filter(_._1.contains(keyword))
    val list     = ListBuffer[Record]()

    for (line <- filtered)
      list += new Record(line._2 + 1, KWICFile.transformContent(line._1))
    this.kwicMap.+=(keyword -> list)
    list
  }

  // Write to the output file following the project's requirements
  // TODO: avoid loops, alphabetical order
  def formatOutput() : String = {
    var ret : String = this.fileName + "\n"
    for ((k,v) <- this.kwicMap) {
      println("Keyword " + k)
      v.foreach( (item) => {
        println(item)
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
  private def transformContent(content : String) : String = {
    val _ret = new StringOps(content)
    _ret.toLowerCase().filter(
      (c) => {
        c match {
          case digitRE(_*)       => false
          case punctuationRE(_*) => false
          case _                 => true
        }
      })
  }
}

object KWICUserInteraction {

  def main(args : Array[String]) : Unit =  {
    
    try {
      val outputFilename               = "output.txt"
      val fileStream                   = scala.io.Source.fromFile("./stop-words.txt")
      val stopWords : Array[String]    = fileStream.getLines.toArray

      fileStream.close()
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
          for (keyword <- stopWords)
            kwicFile.keywordInContext(keyword)
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
