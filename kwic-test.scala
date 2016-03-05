import org.scalatest.FunSuite

// Pietro Paolini - Birkbeck University
// Programming Paradigms 2015/2016 Scala programming coursework

class KWICTest extends FunSuite {

  test("Throw an exception when file is not found") {
    intercept [FileNotFoundException] {
      val error = new KWICFile(Unit)
    }
  }

  test("An easy input file produces a correct output") {
    
    val inputFile      = "./input-test.txt"
    val inputString    = """make a lot of sstuff
I am angry
I do not think that we will zu more than he does
I am here to stay 2 MAKE;;,,
What if I state that I make a lot of stuff in the recent years
What if this is longer than What if I state that I make a lot of stuff in the recent years
"""
    val inputFile      = java.io.File.createTempFile("temp", "test.bbk", new java.io.File("./"))
    val fileWriter     = new FileWriter(inputFile, true)
    val stopWords      = new Array("zu", "make")
    val expectedOutput = """input-test.txt
1                                make a lot of sstuff              
5         what if i state that i make a lot of stuff in the recent 
6 ger than what if i state that  make a lot of stuff in the recent 
3    i do not think that we will zu more than he does
"""
    fileWriter.write(inputString)
    fileWriter.close()
    val kwicFile = new KWICFile(inputFile.getName())
    for (word <- stopWords)
      kwicFile.keywordInContext(word)
    assert(kwicFile.formatOutput, expectedOutput)
    inputFile.delete()
  }

}
