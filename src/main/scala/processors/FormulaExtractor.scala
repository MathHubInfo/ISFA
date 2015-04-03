package processors

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
  val dictionary = Source.fromURL(getClass.getResource("dictionary")).getLines().toSet

  override val skipWhitespace = true

  lazy val oeis_text: PackratParser[String] =
    rep(words | formulas) ^^ {
      case x =>
        x.tail.foldLeft(x.head)((x, y) => {
        if (x.trim.endsWith("$EF$") && y.startsWith("$BF$")) {
          x.trim.dropRight("$EF$".length) + y.drop("$BF$".length)
        } else {
          x + y
        }
      })
    }

  lazy val word: PackratParser[String] = "[A-Za-z]{2,}\\b(?!(\\(|\\{))".r | "([A-Za-z]+\\.+(?!(\\(|\\{)))+".r
  lazy val formula: Regex = ("(.+?)" + delim.regex).r
  lazy val delim: Regex = "((\\.(?:(?=[\\sA-Z\\]\\[\\)\\(\\{\\}])))|([\\,\\!\\?\\s$\\:\\n\\t\\[\\]]))+".r

  lazy val words: PackratParser[String] =
    word ~ opt(delim) ^? {
      case w1~None if(dictionary.contains(w1)) => println("W " + w1); w1+" "
      case w1~Some(a) if(dictionary.contains(w1) || dictionary.contains(w1+a)) => w1 + a + " "
    } |
    "_*[A-Z]+[A-Za-z']+\\b(?!\\()_*".r ^^ /*human names (not functions)*/ {
      x => println("N " + x); x+" "
    }

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

  def parseLine(line : String) = {


  }
  def main(args : Array[String]): Unit = {
    val test = "a(2*n) = A000984(n), a(2*n+1) = A001700(n). "
    println("input : "+ test)
    println(parseAll(oeis_text, test))

  }
}

