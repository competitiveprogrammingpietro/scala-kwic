import scala.collection.mutable.{Map, HashMap}
import scala.collection.mutable.ListBuffer

class Record(line: Int, content: String) {
  val rindex   = line
  val rcontent = content

  override def toString() : String = rindex + " " + rcontent
}

class KWICFile(filename: String) {
  val lines     = scala.io.Source.fromFile(filename).getLines.toArray
  val kwic_map  = new HashMap[String, ListBuffer[Record]]

  def readKeyword(keyword: String) : ListBuffer[Record] =  {
    val filtered = this.lines.zipWithIndex.filter(_._1.contains(keyword))
    var list = ListBuffer[Record]()
    // TODO: Avoid loop
    for (line <- filtered)
      list += new Record(line._2 + 1, line._1)
    kwic_map.+(keyword -> list)
    list
  }

  override def toString(): String = {
    lines.mkString("\n") +
    "\n" +
    kwic_map.values.map(_.toString).mkString("\n")
  }
}

var test = new KWICFile("test.file")
val res = test.readKeyword("match")
//println(res.mkString("\n"))
println(test)
