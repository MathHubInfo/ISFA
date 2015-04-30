package processor

/**
 * Created by enxhi on 4/2/15.
 */

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}


class TextParser extends JavaTokenParsers with PackratParsers {
  var variables = new mutable.HashSet[String]()
  var functions = new mutable.HashSet[String]()
  val dictionary = Source.fromFile("/home/enxhi/github/OEIS_1/src/main/resources/dictionary").getLines().toSet

  override val skipWhitespace = false

  lazy val oeis_text: PackratParser[String] =
    (words) ~> rep1(".*?".r ~ rep(delim)) ^^ {
      case x =>
//        x.tail.foldLeft(x.head)((x, y) => {
//        if (x._2.trim.endsWith("$EF$") && y.startsWith("$BF$")) {
//          x._2.trim.dropRight("$EF$".length) + y.drop("$BF$".length)
//        } else {
//          x + y
//        }
//      })
        println(x)
        x.mkString("")
    }

  lazy val word: PackratParser[String] = "[A-Za-z]+\\b(?!(\\(|\\{))".r | "([A-Za-z]+\\.+(?!(\\(|\\{)))+".r
  lazy val formula: Regex = ("(.+?)" + delim.regex).r
  lazy val delim: Regex = List(" ","\\ " , "\\," , "\\;" , "\\?", "(?<=[A-Za-z\\)])\\:",
    "(?<![A-Z]{1})(?<!\\.)\\.(?!\\d+)(?!\\.)(?![A-Za-z])(?!\\:)").mkString("|").r

  lazy val words: PackratParser[String] =
    word ~ (delim) ^? {
     // case w1~k if(dictionary.contains(w1)) => println("W " + w1); w1+" "
      case w1~(a) if(dictionary.contains(w1) || dictionary.contains(w1+a)) => w1 + a + " "
    } |
    "_*[A-Z]+[A-Za-z']+\\b(?!\\()_*".r ^^ /*human names (not functions)*/ {
      x => println("N " + x); x+" "
    } |
   "([A-Za-z]{2,}\\b(?!(\\(|\\{)))|(([A-Za-z]+\\.+(?!(\\(|\\{)))+)|(_*[A-Z]+[\\ A-Za-z'\\.]+(?!\\()_*)".r ^^ {
     x => println(x); x
   } |
    delim ^^ {x=>println(x); x}


  lazy val dots : PackratParser[String] = "..." | ".."
  lazy val formulas: PackratParser[String] =
    dots ^^ {
      x=> "$BF$" +x+ "$EF$"
    } |
    (formula) ^^ {
      case formula(form, delimiter1, delimiter2, d3) =>
        println(form + " " + delimiter1 +" "+ delimiter2 + d3)
        "$BF$ " + form  + "$EF$" +
          (if(delimiter2 != null) delimiter2 else "") +
          (if(delimiter1 != null) delimiter1 else "")
    }

}


object Parse extends TextParser{

  def main(args : Array[String]): Unit = {
    val test = "where a(2*n) "

    println("input : "+ test)
    println(parseAll(oeis_text, test))

  }
}

