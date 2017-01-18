package sage

import play.api.Logger
import play.api.libs.json.Json

import scala.util.Try
import scalaj.http.Http

import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import com.novus.salat.dao.{DAO, ModelCompanion, SalatDAO}
import com.novus.salat.global.ctx
import org.bson.types.ObjectId
import org.slf4j.LoggerFactory

import parser.{ArgList, Expression, FormulaParserInst}

case class SageRequest(
  request: String,
  result: Option[Expression] // Expression
)

object SageRequest extends ModelCompanion[SageRequest, ObjectId] {
  val mongoClient = MongoClient("localhost", 27017)
  val db = mongoClient("OEIS")

  override def dao: DAO[SageRequest, ObjectId] = new SalatDAO[SageRequest, ObjectId](collection) {}

  def collection = db("sage_cache")

  collection.createIndex(MongoDBObject("request" -> 1))

  def findByRequest(request: String): Option[SageRequest] = {
    findOne(MongoDBObject("request" -> request))
  }
}

object SageWrapper {
  val session = "_ga=GA1.1.1714581731.1463532018; ajs_anonymous_id=%221d80e25a-36a9-4ed6-8dec-d73e9f57670d%22; ajs_group_id=null; ajs_user_id=null; mp_455c026defefc920eae5a5a3a74a9008_mixpanel=%7B%22distinct_id%22%3A%20%22154c14e5b2f4a-0427c3e9dba975-36677f03-13c680-154c14e5b3066e%22%2C%22%24initial_referrer%22%3A%20%22%24direct%22%2C%22%24initial_referring_domain%22%3A%20%22%24direct%22%7D; optimizelyBuckets=%7B%7D; optimizelyEndUserId=oeu1463532355302r0.7610385580606998; optimizelySegments=%7B%223013750511%22%3A%22direct%22%2C%223029780148%22%3A%22false%22%2C%223031480132%22%3A%22gc%22%7D; mjx.menu=renderer%3ACommonHTML; cookie_test_8080=cookie_test; session=\"KJYBg0MI1FwDjyUjr6rn419bbqo=?username=VmFkbWluCnAwCi4=\""

  private var counter = 0

  def integrate(expression: Expression, variables: List[String] = List("x")): Option[Expression] = {
    callMethod(expression, "integrate", variables)
  }

  val logger = LoggerFactory.getLogger(this.getClass)

  private def callMethodWithCall(input: String): Option[ArgList] = {
//    val input = s"$methodName($methodName).$postfixArgument(${postfixArgument.mkString(",")})"
    val responseOpt = SageRequest.findByRequest(input)

    if (responseOpt.isDefined) {
      val result = responseOpt.flatMap(_.result)
      logger.debug(s"Result cached ${result.map(_.toSage)} for $input")
      result.map {
        case s: ArgList => s
        case _ => ArgList(Nil)
      }
    } else {
      logger.debug(s"NOT CACHED")
      try {
        counter += 1

        val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val payload = Map("newcell" -> "0", "id" -> "31", "input" -> input).toSeq
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
          val x = headerSecond.postForm(Map("id" -> "31").toSeq)
          parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
          if ((count % 20) == 1) logger.debug(s"sleeping ... ")
          Thread.sleep(50)
          count += 1
        }
        if (count >= 500) {
          val x = headerFinish.param("_", System.currentTimeMillis().toString)
          logger.debug("Ditching it")
          logger.debug(x.asString.toString)
          logger.debug(x.asString.body)
          logger.debug("\n")
          Thread.sleep(1000)
          headerAlive.param("_", System.currentTimeMillis().toString)
          Thread.sleep(100)
          headerAlive.param("_", System.currentTimeMillis().toString)
          cleanup()
        } else {
          smallCleanup()
          Thread.sleep(20)
        }

        val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "") else "No formula"
        val parsedFormula = FormulaParserInst.parse(formula).orElse {
          logger.debug(s"Couldn't parse: $formula")
          cleanup()
          None
        }
        //    logger.debug(parsedFormula)

        Try {
          SageRequest.save(SageRequest(
            request = input,
            result = parsedFormula
          ))
        }

        parsedFormula.map {
          case s: ArgList => s
          case _ => ArgList(Nil)
        }
      } catch {
        case e: Exception =>
          Logger.debug(s"Got exception $e")
          None
      }
    }
  }

  private def callMethod(expression: Expression, method: String, variables: List[String]) = {
    val input = s"$method(${expression.toSage}, ${variables.mkString(",")})"
    val responseOpt = SageRequest.findByRequest(input)

    if (responseOpt.isDefined) {
      val result = responseOpt.flatMap(_.result)
      logger.debug(s"Result cached ${result.map(_.toSage)} for $input")
      result
    } else {
      logger.debug(s"NOT CACHED")
      try {
        counter += 1

        val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val payload = Map("newcell" -> "0", "id" -> "31", "input" -> input).toSeq
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
          val x = headerSecond.postForm(Map("id" -> "31").toSeq)
          parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
          if ((count % 20) == 1) logger.debug(s"sleeping ... ")
          Thread.sleep(50)
          count += 1
        }
        if (count >= 500) {
          val x = headerFinish.param("_", System.currentTimeMillis().toString)
          logger.debug("Ditching it")
          logger.debug(x.asString.toString)
          logger.debug(x.asString.body)
          logger.debug("\n")
          Thread.sleep(1000)
          headerAlive.param("_", System.currentTimeMillis().toString)
          Thread.sleep(100)
          headerAlive.param("_", System.currentTimeMillis().toString)
          cleanup()
        } else {
          smallCleanup()
          Thread.sleep(20)

        }

        val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "") else "No formula"
        val parsedFormula = FormulaParserInst.parse(formula).orElse {
          logger.debug(s"Couldn't parse: $formula")
          cleanup()
          None
        }
        //    logger.debug(parsedFormula)

        Try {
          SageRequest.save(SageRequest(
            request = input,
            result = parsedFormula
          ))
        }

        parsedFormula
      } catch {
        case e: Exception =>
          Logger.debug(s"Got exception $e")
          None
      }
    }
  }

  def derivative(expression: Expression, variables: List[String] = List("x")): Option[Expression] = {
    callMethod(expression, "derivative", variables)
  }

  def divide(nominator: Expression, denominator: Expression) = {
    callInfixMethod(nominator, "/", denominator)
  }

  private def callInfixMethod(left: Expression, method: String, right: Expression) = {
    val input = s"(${left.toSage}) $method (${right.toSage})"

    val responseOpt = SageRequest.findByRequest(input)

    if (responseOpt.isDefined) {
      val result = responseOpt.flatMap(_.result)
      logger.debug(s"Result cached ${result.map(_.toSage)}for $input")
      result
    } else {
      logger.debug(s"NOT CACHED")
      try {
        counter += 1
        val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val payload = Map("newcell" -> "0", "id" -> "31", "input" -> input).toSeq
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
          val x = headerSecond.postForm(Map("id" -> "31").toSeq)
          parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
          if ((count % 20) == 1) logger.debug(s"sleeping ... ")
          Thread.sleep(50)
          count += 1
        }
        if (count >= 500) {
          val x = headerFinish.param("_", System.currentTimeMillis().toString)
          logger.debug("Ditching it")
          logger.debug(x.asString.toString)
          logger.debug(x.asString.body)
          logger.debug("\n")
          Thread.sleep(1000)
          headerAlive.param("_", System.currentTimeMillis().toString)
          Thread.sleep(100)
          headerAlive.param("_", System.currentTimeMillis().toString)
          cleanup()
        } else {
          smallCleanup()
          Thread.sleep(20)

        }

        val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "")else "No formula"
        val parsedFormula = FormulaParserInst.parse(formula).orElse {
          logger.debug(s"Couldn't parse: $formula")
          cleanup()
          None
        }

        Try {
          SageRequest.save(SageRequest(
            request = input,
            result = parsedFormula
          ))
        }

        parsedFormula
      } catch {
        case e: Exception =>
          Logger.debug(s"Got error $e")
          None
      }
    }
  }

  private def smallCleanup() = {
    try {
      counter += 1
      val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val payload = Map("newcell" -> "0", "id" -> "31", "input" -> s"").toSeq
      headerFirst.postForm(payload).asString.body
      val headerSecond = Http("http://localhost:8080/home/admin/0/cell_update")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      Thread.sleep(20)

      var count = 0
      while (count < 2) {
        val x = headerSecond.postForm(Map("id" -> "31").toSeq)
        (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
        Thread.sleep(50)
        count += 1
      }
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

      val payload = Map("newcell" -> "0", "id" -> "31", "input" -> s"").toSeq
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
      while (parsed == "\" \"" && count < 1) {
        val x = headerSecond.postForm(Map("id" -> "31").toSeq)
        parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
        Thread.sleep(50)
        count += 1
      }
      headerFinish.param("_", System.currentTimeMillis().toString).asString
      Thread.sleep(20)
      headerAlive.param("_", System.currentTimeMillis().toString).asString
    } catch {
      case e: Exception =>
        Logger.debug(s"Got error $e")
        None
    }
  }

  def partialFraction(expression: Expression, variables: List[String] = List("x")): Option[Expression] = {
    callPostfixMethod(expression, "partial_fraction", variables)
  }

  def simplifyFull(expression: Expression): Option[Expression] = {
    callPostfixMethod(expression, "simplify_full", Nil)
  }

  private def callPostfixMethod(expression: Expression, method: String, variables: List[String]) = {
    val input = s"(${expression.toSage}).$method(${variables.mkString(",")})"
    val responseOpt = SageRequest.findByRequest(input)

    if (responseOpt.isDefined) {
      val result = responseOpt.flatMap(_.result)
      logger.debug(s"Result cached ${result.map(_.toSage)}for $input")
      result
    } else {
      logger.debug(s"NOT CACHED")
      try {
        counter += 1
        val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val payload = Map("newcell" -> "0", "id" -> "31", "input" -> input).toSeq
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
          val x = headerSecond.postForm(Map("id" -> "31").toSeq)
          parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
          if ((count % 20) == 1) logger.debug(s"sleeping ... ")
          Thread.sleep(50)
          count += 1
        }
        if (count >= 500) {
          val x = headerFinish.param("_", System.currentTimeMillis().toString)
          logger.debug("Ditching it")
          logger.debug(x.asString.toString)
          logger.debug(x.asString.body)
          logger.debug("\n")
          Thread.sleep(1000)
          headerAlive.param("_", System.currentTimeMillis().toString)
          Thread.sleep(100)
          headerAlive.param("_", System.currentTimeMillis().toString)
          cleanup()
        } else {
          smallCleanup()
          Thread.sleep(20)

        }

        val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "") else "No formula"
        val parsedFormula = FormulaParserInst.parse(formula).orElse {
          logger.debug(s"Couldn't parse: $formula")
          cleanup()
          None
        }


        Try {
          SageRequest.save(SageRequest(
            request = input,
            result = parsedFormula
          ))
        }

        parsedFormula
      } catch {
        case e: Exception =>
          Logger.debug(s"Got error $e")
          None
      }
    }
  }

  def simplify(expression: Expression): Option[Expression] = {
    callMethodNoArguments(expression, "simplify")
  }

  private def callMethodNoArguments(expression: Expression, method: String) = {
    val input = s"$method(${expression.toSage})"

    val responseOpt = SageRequest.findByRequest(input)

    if (responseOpt.isDefined) {
      val result = responseOpt.flatMap(_.result)
      logger.debug(s"Result cached ${result.map(_.toSage)}for $input")
      result
    } else {
      logger.debug(s"NOT CACHED")
      try {
        counter += 1
        val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val payload = Map("newcell" -> "0", "id" -> "31", "input" -> input).toSeq
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
          val x = headerSecond.postForm(Map("id" -> "31").toSeq)
          parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
          if ((count % 20) == 1) logger.debug(s"sleeping ... ")
          Thread.sleep(50)
          count += 1
        }
        if (count >= 500) {
          val x = headerFinish.param("_", System.currentTimeMillis().toString)
          logger.debug("Ditching it")
          logger.debug(x.asString.toString)
          logger.debug(x.asString.body)
          logger.debug("\n")
          Thread.sleep(1000)
          headerAlive.param("_", System.currentTimeMillis().toString)
          Thread.sleep(100)
          headerAlive.param("_", System.currentTimeMillis().toString)
          cleanup()
        }
        val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "") else "No formula"
        val parsedFormula = FormulaParserInst.parse(formula).orElse {
          logger.debug(s"Couldn't parse: $formula")
          cleanup()
          None
        }
        //    logger.debug(parsedFormula)
        Thread.sleep(50)

        smallCleanup()

        Try {
          SageRequest.save(SageRequest(
            request = input,
            result = parsedFormula
          ))
        }

        parsedFormula
      } catch {
        case e: Exception =>
          Logger.debug(s"Got exception $e")
          None
      }
    }
  }


  def oeisTerms(theory: String): Option[ArgList] = {
    val input = s"oeis('$theory').first_terms()"
    callMethodWithCall(input)
  }

  def generatingFunctionSeriesCoefficients(generatingFunction: Expression, terms: Int): Option[ArgList] = {
    val input = s"(${generatingFunction.toSagePython}).series(x,$terms).coefficients()"
    callMethodWithCall(input)
  }

  def oeisOffsets(theory: String): Option[ArgList] = {
    val input = s"oeis('$theory').offsets()"
    callMethodWithCall(input)
  }
}
