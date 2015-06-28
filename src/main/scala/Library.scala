
import java.io.File
import java.net.URL
import java.util.Calendar
import processor.{TextParserIns}

import scala.io.Source
import scala.util.Random
import scala.xml.{Elem, XML}
import parser.{DocumentParser}


object Library {
  //store everything, check before crawling

  private def getURL(entryID : String) : URL = new URL("""http://oeis.org/search?q=id:"""+entryID+"""&fmt=text""")

  //will just give id of number-th OEIS entry
  private def createID( number : String) : String = "A"+"000000".substring(0,6-number.length) + number

  def crawlDocuments(from : Int, to :Int) = {
    if(from < 1){
      throw new Error("There is no entry "+from+" in OEIS!")
    }

    from to to foreach(i =>{
      val theory = createID(i.toString)
      val file = Source.fromURL(getURL(theory))

      printToFile(new File("resources/"+theory)){
        p => file.getLines().foreach(p.println)
      }

      if(i % 10 == 0){
        println("Fetching entry "+ theory)
      }

      file.close()
    })
  }

  def crawlXML(from : Int, to : Int)= {
    if(from < 1){
      throw new Error("There is no entry "+from+" in OEIS!")
    }

    from to to foreach(i =>{
      val theory = createID(i.toString)
      val file = Source.fromURL(getURL(theory))

      val xml = DocumentParser.fromReaderToXML(file)

      if(i % 10 == 0){
        println("Fetching entry "+ theory)
      }

      file.close()
      writeXML(xml, theory)
    })
  }

  def crawlXMLLocal(from : Int, to : Int)= {
    if(from < 1){
      throw new Error("There is no entry "+from+" in OEIS!")
    }

    from to to foreach(i =>{
      val theory = createID(i.toString)
      val fileLoc = "/home/enxhi/projects/OEIST/source/oeis_source/"+theory+".txt"
      val ioFile = new java.io.File(fileLoc)
      if(ioFile.exists) {
        val file = Source.fromFile(ioFile)
        val xml = DocumentParser.fromReaderToXML(file)

        if (i % 1000 == 0) {
          println("Fetching entry " + theory)
        }

        file.close()
        writeXML(xml, theory)
      }else{
        println("File doesn't exists: " + theory)
      }
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
    XML.save("xml_test_paper/" + theory +".omdoc", xml, "UTF-8", true, null)
  }

  def writeFormula(formulas : List[String], theory : String) : Unit = {
    printToFile(new File("xml_out/"+theory)) { p =>
      formulas.foreach(p.println)
    }
  }

  def main(args : Array[String]) = {
//    crawlXMLLocal(1, 3000)
    println(Calendar.getInstance().getTime())
    val documents = 20
    val max = 245000
    val scriptPath = "logs/copyScript"
    val file = new File(scriptPath)
    val rndm = new Random()
    1 to documents foreach { x =>
      val n = rndm.nextInt(max) + 1
      val theory = createID(n.toString)
      printToFile(file)( p => "cp ../../oeis/source/oeis_omdoc/" + theory + " ../source/" )
      crawlXMLLocal(n,n)
    }

    println(TextParserIns.succeded)
    println(TextParserIns.calls)
    println(TextParserIns.exceptions)
    println(Calendar.getInstance().getTime())
  }
}
