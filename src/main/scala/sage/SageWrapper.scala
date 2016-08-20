package sage

import parser.{Expression, FormulaParserInst}
import play.api.Logger
import play.api.libs.json.Json

import scalaj.http.Http

object SageWrapper {
  val session = "_ga=GA1.1.1714581731.1463532018; ajs_anonymous_id=%221d80e25a-36a9-4ed6-8dec-d73e9f57670d%22; ajs_group_id=null; ajs_user_id=null; mp_455c026defefc920eae5a5a3a74a9008_mixpanel=%7B%22distinct_id%22%3A%20%22154c14e5b2f4a-0427c3e9dba975-36677f03-13c680-154c14e5b3066e%22%2C%22%24initial_referrer%22%3A%20%22%24direct%22%2C%22%24initial_referring_domain%22%3A%20%22%24direct%22%7D; optimizelyBuckets=%7B%7D; optimizelyEndUserId=oeu1463532355302r0.7610385580606998; optimizelySegments=%7B%223013750511%22%3A%22direct%22%2C%223029780148%22%3A%22false%22%2C%223031480132%22%3A%22gc%22%7D; mjx.menu=renderer%3ACommonHTML; cookie_test_8080=cookie_test; session=\"j71/vRnsOiLdtsV1hDzJgspoFgY=?username=VmFkbWluCnAwCi4=\""
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


  private def callMethodNoArguments(expression: Expression, method: String) = {
    try {
      counter += 1
      val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val payload = Map("newcell" -> "0", "id" -> "1", "input" -> s"$method(${expression.toSage})").toSeq
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
      Thread.sleep(50)

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

  private def callInfixMethod(left: Expression, method: String, right: Expression) = {
    try {
      counter += 1
      val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val payload = Map("newcell" -> "0", "id" -> "1", "input" -> s"(${left.toSage}) $method (${right.toSage})").toSeq
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

  private def cleanup() = {
    try {
      counter += 1
      val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val payload = Map("newcell" -> "0", "id" -> "1", "input" -> s" ").toSeq
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
      while (parsed == "\" \"" && count < 2) {
        val x = headerSecond.postForm(Map("id" -> "1").toSeq)
        parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
        Thread.sleep(50)
        count += 1
      }
    } catch {
      case e: Exception =>
        Logger.debug(s"Got error $e")
        None
    }
  }

  def integrate(expression: Expression, variables: List[String] = List("x")): Option[Expression] = {
    cleanup()
    callMethod(expression, "integrate", variables)
  }

  def derivative(expression: Expression, variables: List[String] = List("x")): Option[Expression] = {
    cleanup()
    callMethod(expression, "derivative", variables)
  }

  def divide(nominator: Expression, denominator: Expression) = {
    cleanup()
    callInfixMethod(nominator, "/", denominator)
  }

  def partialFraction(expression: Expression, variables: List[String] = List("x")): Option[Expression] = {
    cleanup()
    callPostfixMethod(expression, "partial_fraction", variables)
  }

  def simplifyFull(expression: Expression): Option[Expression] = {
    cleanup()
    callPostfixMethod(expression, "simplify_full", Nil)
  }

  def simplify(expression: Expression): Option[Expression] = {
    cleanup()
    callMethodNoArguments(expression, "simplify")
  }
}
