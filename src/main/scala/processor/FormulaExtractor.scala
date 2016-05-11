package processor

/**
 * Created by enxhi on 4/2/15.
 */

import java.io.Serializable

import com.novus.salat.annotations.raw.Salat
import parser._
import play.api.libs.json.Format

import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
import scala.xml.{Node, Elem}

class TextParser extends FormulaParser {

  def parseLine(line : String, theory : String = "") : Option[Line] = {
    if(line.isEmpty){
      return None
    }

    initSet()
    calls += 1
    try {
      val parsed = parseAll(sentence, line)
      parsed.successful match {
        case false =>
          failFile.println("----------------")
          failFile.write(theory + "\t" + parsed +"\n")
          failFile.println("----------------")
          None
        case true =>
          succeded +=1
          successFile.write(theory + "\t" + line+"\n")
           val processed = parsed.get.parts.map({
            case x : Line => x
            case x : Expression => postProcess(x)
          })
          Some(parsed.get)
      }
    }catch{
      case ex : Throwable =>
        println("ex:" + ex)
        println("line: " + line)
        println("theory: "+ theory)
        exceptions+=1
        exceptionFile.write(theory + "\t" + line + "\n")
        None
    }
  }
}

object TextParserIns extends TextParser{

  def main(args : Array[String]): Unit = {
    val test = "1   - 1    2 "
    println("input : "+ test)
//    println(parseAll(expression, test))
  }

}