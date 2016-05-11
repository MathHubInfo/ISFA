package sage

import play.api.Logger
import com.ning.http.client.AsyncHttpClientConfig
import parser.{FormulaParserInst, Expression}
import play.api.libs.json.Json
import scala.concurrent.ExecutionContext.Implicits.global


import scala.concurrent.Future
import scala.xml.XML
import scalaj.http.Http

object SageWrapper {
  val session = "cookie_test_8080=cookie_test; session=\"h/m2F+jyPm75YsxnWDtDEY3/YwA=?username=VmFkbWluCnAwCi4=\""
  private var counter = 0

  private def callMethod(expression: Expression, method: String, variables: List[String]) = {
    try {
      counter += 1
      val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val payload = Map("newcell" -> "0", "id" -> "1", "input" -> s"$method(${expression.toSage}, ${variables.mkString(",")})").toSeq
      headerFirst.postForm(payload).asString.body
      val headerSecond = Http("http://localhost:8080/home/admin/0/cell_update")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val headerFinish = Http("http://localhost:8080/home/admin/0/discard_and_quit")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val headerAlive = Http("http://localhost:8080/home/admin/0/alive")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      Thread.sleep(20)

      var parsed = "\" \""
      var count = 0
      while (parsed == "\" \"" && count < 500) {
        val x = headerSecond.postForm(Map("id" -> "1").toSeq)
        parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
        if ((count % 20) == 1) println(s"sleeping ... ")
        Thread.sleep(50)
        count += 1
      }
      if (count >= 500) {
        val x = headerFinish.param("_", System.currentTimeMillis().toString)
        println("Ditching it")
        println(x.asString)
        println(x.asString.body)
        println()
        Thread.sleep(1000)
        headerAlive.param("_", System.currentTimeMillis().toString)
        Thread.sleep(100)
      }
      val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8) else "No formula"
      val parsedFormula = FormulaParserInst.parse(formula)
      //    println(parsedFormula)
      Thread.sleep(20)

      parsedFormula
    } catch {
      case e: Exception =>
        Logger.debug(s"Got exception $e")
        None
    }
  }

  private def callPostfixMethod(expression: Expression, method: String, variables: List[String]) = {
    try {
      counter += 1
      val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val payload = Map("newcell" -> "0", "id" -> "1", "input" -> s"(${expression.toSage}).$method(${variables.mkString(",")})").toSeq
      headerFirst.postForm(payload).asString.body
      val headerSecond = Http("http://localhost:8080/home/admin/0/cell_update")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")


      val headerFinish = Http("http://localhost:8080/home/admin/0/discard_and_quit")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val headerAlive = Http("http://localhost:8080/home/admin/0/alive")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      Thread.sleep(20)

      var parsed = "\" \""
      var count = 0
      while (parsed == "\" \"" && count < 500) {
        val x = headerSecond.postForm(Map("id" -> "1").toSeq)
        parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
        if ((count % 20) == 1) println(s"sleeping ... ")
        Thread.sleep(50)
        count += 1
      }
      if (count >= 500) {
        val x = headerFinish.param("_", System.currentTimeMillis().toString)
        println("Ditching it")
        println(x.asString)
        println(x.asString.body)
        println()
        Thread.sleep(1000)
        headerAlive.param("_", System.currentTimeMillis().toString)
        Thread.sleep(100)
      }

      val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8) else "No formula"
      val parsedFormula = FormulaParserInst.parse(formula)
      Thread.sleep(20)

      parsedFormula
    } catch {
      case e: Exception =>
        Logger.debug(s"Got error $e")
        None
    }
  }

  def integrate(expression: Expression, variables: List[String] = List("x")) = {
    callMethod(expression, "integrate", variables)
  }

  def derivative(expression: Expression, variables: List[String] = List("x")) = {
    callMethod(expression, "derivative", variables)
  }

  def partialFraction(expression: Expression, variables: List[String] = List("x")) = {
    callPostfixMethod(expression, "partial_fraction", variables)
  }
}
