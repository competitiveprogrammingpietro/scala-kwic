import scala.collection.mutable.{Map, HashMap}
import scala.collection.mutable.ListBuffer

class Record(line: Int, content: String) {
  val line_index   = line
  val line_content = content
}

class KWICFile(filename: String) {
  val lines     = scala.io.Source.fromFile(filename).getLines.toArray
  val kwic_map  = new HashMap[String, ListBuffer[Record]]

  def readKeyword(keyword: String) : ListBuffer[Record] =  {
    val filtered = this.lines.zipWithIndex.filter(_._1.contains(keyword))
    vat list = ListBuffer[Record]()
    //kwic_map.+(keyword -> filtered)
    filtered
  }

  override def toString(): String = lines.mkString("\n")
}

var test = new KWICFile("test.file")
val res = test.readKeyword("match")
println(res.mkString("\n"))
