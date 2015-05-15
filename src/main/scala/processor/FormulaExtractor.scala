package processor

/**
 * Created by enxhi on 4/2/15.
 */

import parser.{Num, Divisible, Expression, FormulaParser}

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
import scala.xml.Elem

trait Line extends Expression

case class Sentence(parts : List[Expression]) extends Line{
  override def present: String = parts.mkString("")

  override def clear: Expression = this

  //  override def toString = present
  override def toNode(implicit theory: String): Elem =
    <OMOBJ>
      {parts.map(x =>
        x match {
          case a : Line => a.toNode
          case a : Expression => a.toNode
        })
      }
    </OMOBJ>
}

case class Delim(delim : String) extends Line{
  override def present: String = delim

  override def clear: Expression = this

  //  override def toString = present
  //shouldn't be called
  override def toNode(implicit theory: String): Elem = <text>{delim}</text>
}

case class Word(word : String) extends Line{
  override def present: String = word

  override def clear: Expression = this

  //  override def toString = present

  //shouldn't be called
  override def toNode(implicit theory: String): Elem = <text>{word}</text>
}

case class Name(name : String) extends Line{
  override def present: String = name

  override def clear: Expression = this

  //  override def toString = present
  //shouldn't be called
  override def toNode(implicit theory: String): Elem = <text>{name}</text>
}

case class Date(date : String) extends Line{
  override def present: String = date

  override def clear: Expression = this

  //  override def toString = present
  //shouldn't be called
  override def toNode(implicit theory: String): Elem = <text>{date}</text>
}

case class Email(email : String) extends Line{
  override def present: String = email

  override def clear: Expression = this

  override def toNode(implicit theory: String): Elem = <text>{email}</text>
}

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
          Some(Sentence(processed))
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
    val test = " (-1)^n"
    println("input : "+ test)
    println(parseAll(sentence, test))
  }

}