package processor

/**
 * Created by enxhi on 4/2/15.
 */

import parser._

class TextParser extends FormulaParser {
  /**
   * take the OEIS function, parse it e.g.
   * from "Number of groups of order n."
   * to "[1.30] parsed: Sentence(List(Name(Number), Word(of), Word(groups), Word(of), Word(order), Var("n"), Delim(.)))"
   *
   * @param line line: literally a line from OEIS, either the name or the formula field
   * @param theory a_num as string e.g. "A000001"
   * @return the line, but parsed
   */
  def parseLine(line : String, theory : String = "") : Option[Line] = {
    if(line.isEmpty){
      return None
    }

    initSet()
    calls += 1
    try {
      // sentence is a global variable in FormularParser.scala
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
    val test = "G.f.: x*(1+2*x)*(1+x^2)/(1-x^2)^2. - Ralf Stephan, Jun 10 2003"
    println("input : "+ test)
    println(parseLine(test).get)
  }

}