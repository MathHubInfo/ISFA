
import java.io.File
import java.net.URL
import scala.io.Source
import scala.xml.{Elem, XML}
import parser.{FormulaParser, DocumentParser}


object Library {
  //store everything, check before crawling

  private def getURL(entryID : String) : URL = new URL("""http://oeis.org/search?q=id:"""+entryID+"""&fmt=text""")

  //will just give id of number-th OEIS entry
  private def createID( number : String) : String = "A"+"000000".substring(0,6-number.length) + number

  def crawlXML(from : Int, to : Int)= {
    if(from < 1){
      throw new Error("There is no entry "+from+" in OEIS!")
    }

    from to to foreach(i =>{
      val theory = createID(i.toString)
      val xml = DocumentParser.fromReaderToXML(Source.fromURL(getURL(theory)))

      if(i % 10 == 0){
        println("Fetching entry "+ theory)
      }

      writeXML(xml, theory)
    })
  }

  def crawlText(from : Int, to : Int) = {
    if(from < 1){
      throw new Error("There is no entry "+from+" in OEIS!")
    }

    from to to foreach(i =>{
      val theory = createID(i.toString)
      val doc: List[String] = DocumentParser.getFormulas(Source.fromURL(getURL(theory)))

      if(i % 10 == 0){
        println("Fetching entry "+ theory)
      }

      writeFormula(doc, theory)
    })
  }
  def getXML(entry : Int) : Elem = {
    val id = createID(entry.toString)
//    if(storage.get(id).isEmpty){
      DocumentParser.fromReaderToXML(Source.fromURL(getURL(id)))
//    }else{
//      storage.get(id).get.toNode
//    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val print = new java.io.PrintWriter(f)
    try {
      op(print)
    } finally {
      print.close()
    }
  }

  def writeXML(xml : Elem, theory : String) = {
    //XML.save("xml_out/" + theory +".omdoc", xml, "UTF-8", true, null)
  }

  def writeFormula(formulas : List[String], theory : String) : Unit = {
    printToFile(new File("out/xml_out"+theory)) { p =>
      formulas.foreach(p.println)
    }
  }

  def main(args : Array[String]) = {
    crawlXML(1, 4000)
    println(FormulaParser.succeded)
    println(FormulaParser.calls)
    println(FormulaParser.exceptions)
  }
}
