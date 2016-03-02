import scala.collection.mutable.{Map, HashMap,ListBuffer}
import scala.collection.immutable.StringOps
import java.io.PrintWriter

class Record(line: Int, content: String) {
  val rindex   = line
  val rcontent = content

  override def toString() : String = "Line:" + rindex + " Content: " + rcontent
}

class KWICFile(filename: String) {
  val fileName  = filename

  // TODO: close file descriptor
  val lines     = scala.io.Source.fromFile(filename).getLines.toArray
  val kwicMap   = new HashMap[String, ListBuffer[Record]]
 
  // keyword must be lower case
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
  // TODO: avoid loops
  private def formatOutput() : String = {
    var ret : String = this.fileName + "\n"
    for ((k,v) <- this.kwicMap) {
      v.foreach( (item) => {
        ret += item.rindex + " "
        ret += item.rcontent + "\n"
      })
    }
    ret
  }

  def writeToFile(output : PrintWriter) : Unit = {
    output.println(this.formatOutput)
  }

  override def toString(): String = {
    lines.mkString("\n") +
    kwicMap.values.map(_.toString).mkString("\n") +
    this.formatOutput
  }
}

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

  // TODO: exception if file not found, what do to ?
  val stopWords : Array[String]    = scala.io.Source.fromFile("./stop-words.txt").getLines.toArray
  val files : ListBuffer[KWICFile] = ListBuffer()
  
  def readFile() : Unit = {
    var path : String = ""
    val lines = Iterator.continually(readLine()).takeWhile(_ != null).mkString
  }
}

var test = new KWICFile("test.file")
val res = test.keywordInContext("match")
//println(res.mkString("\n"))
println(test)
KWICUserInteraction.readFile
