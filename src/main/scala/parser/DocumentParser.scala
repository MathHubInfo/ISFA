package parser

import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.Imports._

import com.novus.salat.annotations._
import com.novus.salat.dao.{DAO, ModelCompanion, SalatDAO}
import library.Library
import org.bson.types.ObjectId
import org.json4s.{NoTypeHints, ShortTypeHints}
import org.json4s.native.Serialization
import parser.DocumentParser.GeneratingFunctionDefinition
import processor.{TextParserIns, TextParser}
import com.novus.salat.global.ctx

import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import scala.xml._

import org.json4s.native.Serialization._

case class Theory(
  @Key("_id") id: ObjectId = new ObjectId(),
  theory: String,
  name: Option[String],
  formulas: Seq[String],
  var generatingFunctions: Seq[String] = List(),
  var pureGeneratingFunctions: Seq[String] = List(),
  var partialFractions: Seq[String] = List(),
  var sage: List[String] = List(),
  var pureGeneratingFunctionsPartialized: List[String] = List(),
  var sageUnified: List[String] = List()
){
//  override def toString() = s"$theory \n ${generatingFunctions.map(_.toString + "\n")}"
}

case class PartialFractionToTransforms(expression: String, transforms: List[List[String]])
object PartialFractionToTransforms {
  val hints = ShortTypeHints(List(classOf[PartialFractionToTransforms], classOf[Expression], classOf[String]))
  implicit val ptformat = Serialization.formats(hints)
}

case class PartialFractionAndTransform(expression: Expression, transform: String)
object PartialFractionAndTransform {
  import Expression.format
  val hints = ShortTypeHints(List(classOf[PartialFractionAndTransform], classOf[Expression], classOf[String]))
  implicit val pformat = Serialization.formats(hints)
}

object DocumentDao extends ModelCompanion[Theory, ObjectId] {
  val mongoClient = MongoClient("localhost", 27017)
  val db = mongoClient("OEIS")
  def collection = db("theory")
  override def dao: DAO[Theory, ObjectId] = new SalatDAO[Theory, ObjectId](collection) {}

  def findOneByTheory(theoryNumber: Int): Option[Theory] = {
    val theory = Library.createID(theoryNumber.toString)

    dao.findOne(MongoDBObject("theory" -> theory))
  }

  def removeOneByTheory(theory: String) = {
    dao.findOne(MongoDBObject("theory" -> theory)).map(dao.remove)
  }

  def updateDao(theory: Theory) = {
    dao.removeById(theory.id)
    dao.insert(theory)
  }
}


object DocumentParser {
  val dictionary = Source.fromFile(getClass.getResource("/dictionary").getPath).getLines().toSet

  private val IDregex = "A\\d+".r

  private def assertion(xclass : String, cmpval : String) : Elem ={
    <assertion class={xclass}>
      {CMP(cmpval)}
    </assertion>
  }

  private def omtext(xclass : String, cmpval : String) : Elem = {
    <omtext class={xclass}>
      {CMP(cmpval)}
    </omtext>
  }

  private def CMP(value : String) : Elem = <CMP>{value}</CMP>

  private def omdoc(element : Elem) : Elem = <omdoc:p>{element}</omdoc:p>

  private def getTheoryID(line : String) : List[String] ={
    (IDregex findAllIn  line).toList
  }

  def addHeaders(xml : List[Elem], theory : String) : Elem = {
    <omdoc xmlns="http://omdoc.org/ns" xmlns:omdoc="http://omdoc.org/ns" xmlns:html="http://www.w3.org/1999/xhtml" xml:id={theory+".omdoc"}>
      <!--This OMDoc file is generated from an OEIS entry, do not edit-->
      <theory name={theory}>
        {xml}
      </theory>
    </omdoc>
  }

  def omdocWrapperCMP(xclass : String, cmpval : String) = omtext(xclass, cmpval)
  def omdocWrapperAs(xclass : String, cmpval : String) = assertion(xclass, cmpval)

  def parseLines(documentLines : List[String] ) : Elem = {
    var theory : Option[String] = None

    val xml: List[Any] = documentLines.collect({
      case line if line.length > 2 =>
        val contentIndex: Option[Match] = IDregex.findFirstMatchIn(line)

        if(!contentIndex.isEmpty && theory.isEmpty){
          theory = Some(contentIndex.get.matched)
        }

        line.substring(0,2) match{
          case "%N" =>  omdocWrapperCMP("name", line.substring(contentIndex.get.end))
          case "%S" =>  omdocWrapperCMP("starts-with", line.substring(contentIndex.get.end))
          case "%C" =>  omdocWrapperCMP("comment", line.substring(contentIndex.get.end))
          case "%D" =>  omdocWrapperCMP("reference", line.substring(contentIndex.get.end))
          case "%H" =>  omdocWrapperCMP("link", line.substring(contentIndex.get.end))
          case "%F" =>  formulaWrap(line.substring(contentIndex.get.end), theory.get)
          case "%Y" =>  omdocWrapperAs("crossref", line.substring(contentIndex.get.end))
          case "%K" =>  omdocWrapperAs("keywords", line.substring(contentIndex.get.end))
          case "%A" =>  omdocWrapperAs("author", line.substring(contentIndex.get.end))
          case "%p" =>  omdocWrapperCMP("maple", line.substring(contentIndex.get.end))
          case "%t" =>  omdocWrapperCMP("mathematica", line.substring(contentIndex.get.end))
          case "%o" =>  omdocWrapperCMP("program", line.substring(contentIndex.get.end))
          case "%O" =>  omdocWrapperAs("offset", line.substring(contentIndex.get.end))
          case "%E" =>  omdocWrapperAs("extensions", line.substring(contentIndex.get.end))
          case "%e" =>  omdocWrapperAs("example", line.substring(contentIndex.get.end))
          case "%T" =>  omdocWrapperAs("***** UUUU *****", line.substring(contentIndex.get.end))
          case "%U" =>  omdocWrapperAs("***** IIII *****", line.substring(contentIndex.get.end))
          case "%I" =>  omdocWrapperAs("***** TTTT *****", line.substring(contentIndex.get.end))
          case a if line.startsWith("%") => omdocWrapperAs("notsupported","Unexpected tag!")
          case _ =>
        }
    })

    addHeaders(xml collect {case a : Elem => a}, theory.get)
  }

  def parseLinesTheory(documentLines : List[String] ) : Theory = {
    var theory : Option[String] = None
    var name: Option[String] = None

    val formulas: collection.mutable.ListBuffer[Expression] = collection.mutable.ListBuffer.empty
    val xml: List[Any] = documentLines.collect({
      case line if line.length > 2 =>
        val contentIndex: Option[Match] = IDregex.findFirstMatchIn(line)

        if(!contentIndex.isEmpty && theory.isEmpty){
          theory = Some(contentIndex.get.matched)
        }

        line.substring(0,2) match{
          case "%N" =>  name = Some(line.substring(contentIndex.get.end))
          case "%F" => TextParserIns.parseLine(line.substring(contentIndex.get.end), theory.get).foreach {x => formulas += x}
          case _ =>
        }
    })

    import Expression.format
    Theory(theory = theory.get, name = name, formulas = formulas.toSeq.map(x => write(x)))
  }

  def fromReaderToXML(source : BufferedSource) : Elem = {
    parseLines(source.getLines().toList)
  }

  def parseDocument(document : String) : String = {
    parseLines(document.lines.toList).toString
  }

  case class GeneratingFunctionDefinition(function: Func, body: Expression)
  def getGeneratingFunction(expression: Expression): List[GeneratingFunctionDefinition] = {
    expression match {
      case Sentence(exps) => exps.flatMap(getGeneratingFunction)
      case in: GeneratingFunction => List(GeneratingFunctionDefinition(Func("GF", ArgList(List(Var("x")))), in.expression))
      case Equation(eq, GeneratingFunctionDef(exp), definition) if eq.trim == "=" => List(GeneratingFunctionDefinition(Func("GF", exp), definition))
      case _ => List()
    }
  }

  def split(s:String, l:List[String]):List[(String,String)] = {
    if(s == null){
      return Nil
    }

    val delimRegex = l.mkString("|")
    val r = "("+delimRegex+")?(.*?)(("+delimRegex+").*)?"
    val R = r.r

    s match {
      case R(delim, text, rest, _) if delim == null => (text, "") :: split(rest, l)
      case R(delim, text, rest, _) => (text, delim) :: split(rest, l)
      case x => Nil
    }
  }

  def extractFormula(line : String): (List[String], List[Int]) = {
    val month = "(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"

    /*Month day year*/
    val date = month + "\\s*\\d{0,4}\\s*\\d{0,4}"

    val delimiters = List(date, " " ,
      ":",
      "(?<![A-Z]{1})(?<!\\.)[\\.\\;,](?!\\d+)(?!\\.)(?![A-Za-z\\:])"
    )

    /*More than 2 characters not followed by any kind of open paran
    * OR _NAME's Surname_
    * */
    val word: Regex = "([A-Za-z]{2,}\\b(?!(\\(|\\{\\[)))|(([A-Za-z]+\\.+(?!(\\(|\\{\\[)))+)|(_*[A-Z]+[\\sA-Za-z'\\.]+(?!\\()_*)".r

    /*words that should not be searched in a dictionary, they are not words by definition*/
    val funcs : List[String] = List("mod", "pi", "phi", "log", "divide", "divides")
    var balancedPar = 0

    def isWord(token : String, dropped : Boolean) : Boolean = {
      if(dropped && token.trim == "-") return true
      if(funcs.contains(token.trim)) return false

      val result = dictionary.contains(token.trim) ||
        delimiters.exists(x => token.matches(x)) ||
        token.matches(word.regex) ||
        token.trim.matches(word.regex) ||
        delimiters.exists(x => token.matches(x.trim)) ||
        token.trim == ""

      /*Detect words of the form -- (continues here) OR bla (is greater) */
      if(token.length > 4 && (token(0) == '(' || token(token.length-1) == ')')){
        var newToken = token
        if(newToken(0) == '('){
          newToken = newToken.substring(1)
        }
        if(newToken(newToken.length-1) == ')'){
          newToken = newToken.dropRight(1)
        }
        return result || isWord(newToken, dropped)
      }

      result
    }

    def isDelim(token : String) : Boolean = {
      delimiters.exists(x => token.matches(x.trim)) || delimiters.exists(x => token.matches(x))
    }

    def computeBalancedPar(word : String) : Int = {
      word.count(_ == '(') - word.count(_ == ')') +
        word.count(_ == '{') - word.count(_ == '}') +
        word.count(_ == '[') - word.count(_ == ']')
    }

    //contains pairs (word, delimiter) - delimiters will be needed to put the line together again
    var words: List[(String,String)] = split(line, delimiters )
    var transformed : List[String] = Nil
    var formulaPosition : List[Int] = Nil

//    println(words)
    var temp : List[String] = Nil
    var delims : List[String] = Nil

    //was a "-" dropped in the last formula try
    var dropped = false

    //Take the words first, when you can't see words anymore start taking formulas until you spot a word
    while(words.nonEmpty) {

      while (words.nonEmpty && isWord(words.head._1, dropped)) {
        balancedPar += computeBalancedPar(words.head._1)
        temp = temp :+ words.head._1
        delims = delims :+ words.head._2
        words = words.tail
        dropped = false
      }

      transformed = transformed :+ temp.zip(delims).map( wordDelim => wordDelim._2 + wordDelim._1).mkString("")

      if(words.nonEmpty){
        temp = Nil
        delims = Nil

        /*if we have a sum(x, then we know that the comma doesn't finish it (unbalanced parans)
        * OR prod(x=1..t: the colon doesn't finish the formula
        * */
        var isFormulaUnfinished = false

        while(words.nonEmpty && (!isWord(words.head._1, dropped) || isFormulaUnfinished)){
          balancedPar += computeBalancedPar(words.head._1)
          isFormulaUnfinished = false
          if(temp.length == 0 && words.head._2.trim == ":"){
            delims = delims :+ ""
          }else {
            delims = delims :+ words.head._2
          }
          temp = temp :+ words.head._1
          words = words.tail

          //not checking before while loop because the comma can only be after at least one expr.
          if(words.nonEmpty) {
            //if there is a comma inside two parans then it is part of the formula
            if ((words.head._2 == "," || words.head._2 == ":") && balancedPar > 0) {
              isFormulaUnfinished = true
            }
          }
        }

        //deal with sentences like x+23+a(x) - Euler
        if(temp.last == "-"){
          words = (temp.last, delims.last)::words
          temp = temp.dropRight(1)
          delims = delims.dropRight(1)
          dropped = true
        }


        transformed = transformed :+ temp.zip(delims).map( wordDelim => wordDelim._2 + wordDelim._1).mkString("")
        formulaPosition = formulaPosition :+ (transformed.length -1)
      }

      temp = Nil
      delims = Nil
    }

    transformed -> formulaPosition
  }


  private def parsedFormulaWrap(expr :Expression, theory : String) : Elem= {
    <OMOBJ>
      {expr.toNode(theory)}
    </OMOBJ>
  }

  def formulaWrap(line : String, theory : String ) : Elem = {
    <omdoc:p class="formula">
        {TextParserIns.parseLine(line, theory) match {
          case Some(a) => a.toNode(theory)
          case None => <CMP>{line}</CMP>
          }
        }
    </omdoc:p>
  }

  def main(args : Array[String]) : Unit = {
    val res = extractFormula("Recurrence:A(x) + 3 + Prod_{i=1..t: t^2+4}. - Elhaida, Mar 20, 1994 ")
//    println(res)
//    println(res._1.zipWithIndex)
  }

}
