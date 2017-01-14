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

import parser.{Expression, FormulaParserInst}

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
  val session = "nb_session_8080=; session=\"1qb/9Auy7FvJOif55F+uodnt+Bw=?username=UydhZG1pbicKcDAKLg==\""

  private var counter = 0

  def integrate(expression: Expression, variables: List[String] = List("x")): Option[Expression] = {
    callMethod(expression, "integrate", variables)
  }

  val logger = LoggerFactory.getLogger(this.getClass)

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
        }
        val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "") else "No formula"
        val parsedFormula = FormulaParserInst.parse(formula)
        //    logger.debug(parsedFormula)
        Thread.sleep(20)

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
        }

        val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "")else "No formula"
        val parsedFormula = FormulaParserInst.parse(formula)
        Thread.sleep(20)

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

  private def cleanup() = {
    try {
      counter += 1
      val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val payload = Map("newcell" -> "0", "id" -> "31", "input" -> s" ").toSeq
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
        }

        val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "") else "No formula"
        val parsedFormula = FormulaParserInst.parse(formula)
        Thread.sleep(20)

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
        val parsedFormula = FormulaParserInst.parse(formula)
        //    logger.debug(parsedFormula)
        Thread.sleep(50)


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
}
