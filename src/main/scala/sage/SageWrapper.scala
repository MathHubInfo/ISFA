package sage

import com.mongodb.casbah.MongoClient
import com.mongodb.casbah.commons.MongoDBObject
import org.bson.types.ObjectId
import org.slf4j.LoggerFactory
import parser.{ArgList, Expression, FormulaParserInst}
import play.api.libs.json.Json
import salat.dao.{DAO, ModelCompanion, SalatDAO}
import salat.global.ctx
import scalaj.http.Http

import scala.util.Try

import java.net._
import java.io._
import scala.io._

case class SageRequest(
                        request: String,
                        result: Option[Expression] // Expression
                      )

/* Notes on this file:
*  <SageMath Server Connection>
*     Http(sagemath_server_http+"eval") looks like it connects to the local SageMath Server Enxhell had
*     on his machine. Then he has a folder "0" containing various folders. I'm going to try to make that work now.
*     changed:
*             val headerFirst = Http("http://localhost:8080/home/admin/0/eval")
*             val headerFirst = Http(sagemath_server_http + "eval")
*     while having a SageMath Jupyter open, it at least finds the socket, but fails on the html?
*     I'm currently trying to get the Sage cmd to host something.
*
*
*
* */

// MongoDB access to OEIS/sage_cache, port is default
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

// SageMath stuff
object SageWrapper {
  // This seems to be a session cookie for SageMath
  //val session = "nb_session_8080=; session=\"dZMN57/0ShESsTbOvrba9uGuPag=?username=UydhZG1pbicKcDAKLg==\""
  val session = "session=\"5ec459db53071712c94020d847a93d822dddd34f915e6df1\""
  // val sagemath_server_http = "http://localhost:8888/?token=f0d0814871607024f779691000052fe6bdeabc18dfbc89f3"
  // Jupyter notebook path to the desired folder
  val sagemath_server_http = "http://127.0.0.1:8888/notebooks/Desktop/ISFA_sagemath.ipynb#"

  private var counter = 0

  def integrate(expression: Expression, variables: List[String] = List("x")): Option[Expression] = {
    callMethod(expression, "integrate", variables)
  }

  val logger = LoggerFactory.getLogger(this.getClass)

  private def callMethodWithCall(input: String): Option[ArgList] = {
    logger.debug(s"xxx callMethodWithCall")
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
      logger.debug(s"NOT CACHED $input")
      try {
        counter += 1

        val headerFirst = Http(sagemath_server_http + "eval")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")


        val savePayload = Map("save_only" -> "1", "id" -> "31", "input" -> input).toSeq
        headerFirst.postForm(savePayload).asString.body

        val payload = Map("newcell" -> "0", "id" -> "31", "input" -> input).toSeq
        headerFirst.postForm(payload).asString.body
        val headerSecond = Http(sagemath_server_http + "cell_update")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val headerFinish = Http(sagemath_server_http + "discard_and_quit")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val headerAlive = Http(sagemath_server_http + "alive")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        Thread.sleep(20)

        var parsed = "\" \""
        var count = 0
        while (parsed == "\" \"" && count < 500) {
          val x = headerSecond.postForm(Map("id" -> "31").toSeq)
          parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString()
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

          None
        } else {
          smallCleanup()
          Thread.sleep(20)

          val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "") else "No formula"
          println("Parsed: " + formula.drop(1).dropRight(1).split(",").map(_.trim.toDouble).toList)
          val parsedFormula = FormulaParserInst.parse(formula).orElse {
            logger.debug(s"Couldn't parse: $formula")
            None
          }
          //    logger.debug(parsedFormula)

          Try {
            SageRequest.save(SageRequest(
              request = input,
              result = parsedFormula
            ))
            cleanup()
          }

          parsedFormula.map {
            case s: ArgList => s
            case _ => ArgList(Nil)
          }
        }
      } catch {
        case e: Exception =>
          logger.debug(s"Got exception $e")
          None
      }
    }
  }

  // almost the same too, except val savePayload
  private def callMethod(expression: Expression, method: String, variables: List[String]) = {
    logger.debug(s"xxx callMethod")
    val input = s"$method(${expression.toSage}, ${variables.mkString(",")})"
    val responseOpt = SageRequest.findByRequest(input)

    if (responseOpt.isDefined) {
      val result = responseOpt.flatMap(_.result)
      logger.debug(s"Result cached ${result.map(_.toSage)} for $input")
      result
    } else {
      logger.debug(s"NOT CACHED $input")
      try {
        counter += 1

        val headerFirst = Http(sagemath_server_http + "eval")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")
        val savePayload = Map("save_only" -> "1", "id" -> "31", "input" -> input).toSeq
        headerFirst.postForm(savePayload).asString.body

        val payload = Map("newcell" -> "0", "id" -> "31", "input" -> input).toSeq
        headerFirst.postForm(payload).asString.body
        val headerSecond = Http(sagemath_server_http + "cell_update")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val headerFinish = Http(sagemath_server_http + "discard_and_quit")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val headerAlive = Http(sagemath_server_http + "alive")
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
          None
        }
        //    logger.debug(parsedFormula)

        if (count < 500) {
          Try {
            SageRequest.save(SageRequest(
              request = input,
              result = parsedFormula
            ))
          }
          cleanup()
        }

        parsedFormula
      } catch {
        case e: Exception =>
          logger.debug(s"Got exception $e")
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

  // ToDo: also almost identical to callMethodNoArguments, except the var "savePayload"
  private def callInfixMethod(left: Expression, method: String, right: Expression) = {
    logger.debug(s"xxx callMethodWithCall")
    val input = s"(${left.toSage}) $method (${right.toSage})"

    val responseOpt = SageRequest.findByRequest(input)

    if (responseOpt.isDefined) {
      val result = responseOpt.flatMap(_.result)
      logger.debug(s"Result cached ${result.map(_.toSage)}for $input")
      result
    } else {
      logger.debug(s"NOT CACHED $input")
      try {
        counter += 1
        val headerFirst = Http(sagemath_server_http + "eval")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val savePayload = Map("save_only" -> "1", "id" -> "31", "input" -> input).toSeq
        headerFirst.postForm(savePayload).asString.body

        val payload = Map("newcell" -> "0", "id" -> "31", "input" -> input).toSeq
        headerFirst.postForm(payload).asString.body
        val headerSecond = Http(sagemath_server_http + "cell_update")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")


        val headerFinish = Http(sagemath_server_http + "discard_and_quit")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val headerAlive = Http(sagemath_server_http + "alive")
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
          None
        }

        if (count < 500) {
          Try {
            SageRequest.save(SageRequest(
              request = input,
              result = parsedFormula
            ))
          }
          cleanup()
        }

        parsedFormula
      } catch {
        case e: Exception =>
          logger.debug(s"Got error $e")
          None
      }
    }
  }

  // This cleans the Jupyter notebook ToDo: what else?
  private def smallCleanup() = {
    logger.debug(s"xxx smallCleanup")
    try {
      counter += 1
      val headerFirst = Http(sagemath_server_http + "eval")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val savePayload = Map("save_only" -> "1", "id" -> "31", "input" -> s"").toSeq
      headerFirst.postForm(savePayload).asString.body

      val payload = Map("newcell" -> "0", "id" -> "31", "input" -> s"").toSeq
      headerFirst.postForm(payload).asString.body
      val headerSecond = Http(sagemath_server_http + "cell_update")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      Thread.sleep(50)

      var count = 0
      var parsed = "f"
      while (count < 4 || (parsed != "\" \"" && count < 50)) {
        val x = headerSecond.postForm(Map("id" -> "31").toSeq)
        parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
        Thread.sleep(50)
        count += 1
      }
    } catch {
      case e: Exception =>
        logger.debug(s"Got error $e")
        None
    }
    //    cleanup()
  }

  // This cleans the Jupyter notebook ToDo: what else?
  private def cleanup() = {
    logger.debug(s"xxx cleanup")
    try {
      counter += 1
      val headerFirst = Http(sagemath_server_http + "eval")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val savePayload = Map("save_only" -> "1", "id" -> "31", "input" -> "").toSeq
      headerFirst.postForm(savePayload).asString.body

      val payload = Map("newcell" -> "0", "id" -> "31", "input" -> s"").toSeq
      headerFirst.postForm(payload).asString.body
      val headerSecond = Http(sagemath_server_http + "cell_update")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")


      val headerFinish = Http(sagemath_server_http + "discard_and_quit")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      val headerAlive = Http(sagemath_server_http + "alive")
        .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
          "Cookie" -> session
          , "Accept" -> "text/plain")

      Thread.sleep(20)

      var parsed = "\" \""
      var count = 0
      while (parsed == "\" \"" && count < 2) {
        val x = headerSecond.postForm(Map("id" -> "31").toSeq)
        parsed = (Json.parse(x.asString.body) \ "output_wrapped").get.toString().trim
        Thread.sleep(50)
        count += 1
      }
      headerFinish.param("_", System.currentTimeMillis().toString).asString
      Thread.sleep(700)
      headerAlive.param("_", System.currentTimeMillis().toString).asString
      Thread.sleep(300)
    } catch {
      case e: Exception =>
        logger.debug(s"Got error $e")
        None
    }
  }

  def partialFraction(expression: Expression, variables: List[String] = List("x")): Option[Expression] = {
    PythonSageServerPostFixMethod(expression, "partial_fraction", variables)
    //callPostfixMethod(expression, "partial_fraction", variables)
  }

  def simplifyFull(expression: Expression): Option[Expression] = {
    PythonSageServerPostFixMethod(expression, "simplify_full", Nil)
    //callPostfixMethod(expression, "simplify_full", Nil)
  }

  // ToDo the server has to close the connection or this will get stuck (it still gets stuck?)
  private def PythonSageServerPostFixMethod(expression: Expression, method: String, variables: List[String]) = {
    logger.debug(s"\nsagemath server reqest")

    // the sage command e.g. "((x/(95-(15*x))^5)).simplify_full()"
    val input = s"(${expression.toSage}).$method(${variables.mkString(",")})"
    val responseOpt = SageRequest.findByRequest(input)

    if (responseOpt.isDefined) {
      val result = responseOpt.flatMap(_.result)
      logger.debug(s"Result cached ${result.map(_.toSage)}for $input")
      result
    } else {
      logger.debug(s"NOT CACHED $input")
      try {
        val s = new Socket(InetAddress.getByName("127.0.0.1"), 65432)
        lazy val in = new BufferedSource(s.getInputStream()).getLines()
        val out = new PrintStream(s.getOutputStream())

        out.println(s"$input")
        out.flush()

        val parsed = in.mkString("\n")

        s.close()

        //Thread.sleep(20)

        logger.debug(s"Parsing $parsed")
        val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "") else "No formula"
        val parsedFormula = FormulaParserInst.parse(formula).orElse {
          logger.debug(s"Couldn't parse: $formula")
          None
        }

        try {
          SageRequest.save(SageRequest(
            request = input,
            result = parsedFormula
          ))
        }
        //cleanup()


        parsedFormula

        None
      } catch {
        case e: Exception =>
          logger.debug(s"Got error $e")
          None
      }
    }
  }

  // only one called by GeneratingFunctionsSagePackage.generateForAll()
  private def callPostfixMethod(expression: Expression, method: String, variables: List[String]) = {
    logger.debug(s"xxx callPostfixMethod")
    val input = s"(${expression.toSage}).$method(${variables.mkString(",")})"
    val responseOpt = SageRequest.findByRequest(input)

    if (responseOpt.isDefined) {
      val result = responseOpt.flatMap(_.result)
      logger.debug(s"Result cached ${result.map(_.toSage)}for $input")
      result
    } else {
      logger.debug(s"NOT CACHED $input")


      try {
        counter += 1
        val headerFirst = Http(sagemath_server_http + "eval")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val payload = Map("newcell" -> "0", "id" -> "31", "input" -> input).toSeq
        headerFirst.postForm(payload).asString.body
        val headerSecond = Http(sagemath_server_http + "cell_update")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")


        val headerFinish = Http(sagemath_server_http + "discard_and_quit")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val headerAlive = Http(sagemath_server_http + "alive")
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

        logger.debug(s"Parsing $parsed")
        val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "") else "No formula"
        val parsedFormula = FormulaParserInst.parse(formula).orElse {
          logger.debug(s"Couldn't parse: $formula")
          None
        }


        if (count < 500) {
          Try {
            SageRequest.save(SageRequest(
              request = input,
              result = parsedFormula
            ))
          }
          cleanup()
        }

        parsedFormula
      } catch {
        case e: Exception =>
          logger.debug(s"Got error $e")
          None
      }
    }
  }

  def simplify(expression: Expression): Option[Expression] = {
    callMethodNoArguments(expression, "simplify")
  }

  //this is almost identical to callPostfixMethod()?
  private def callMethodNoArguments(expression: Expression, method: String) = {
    logger.debug(s"xxx callMethodNoArguments")
    val input = s"$method(${expression.toSage})"

    val responseOpt = SageRequest.findByRequest(input)

    if (responseOpt.isDefined) {
      val result = responseOpt.flatMap(_.result)
      logger.debug(s"Result cached ${result.map(_.toSage)}for $input")
      result
    } else {
      logger.debug(s"NOT CACHED $input")
      try {
        counter += 1
        val headerFirst = Http(sagemath_server_http + "eval")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val payload = Map("newcell" -> "0", "id" -> "31", "input" -> input).toSeq
        headerFirst.postForm(payload).asString.body
        val headerSecond = Http(sagemath_server_http + "cell_update")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val headerFinish = Http(sagemath_server_http + "discard_and_quit")
          .headers("Content-Type" -> "application/x-www-form-urlencoded; charset=UTF-8",
            "Cookie" -> session
            , "Accept" -> "text/plain")

        val headerAlive = Http(sagemath_server_http + "alive")
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

        logger.debug(s"Parsing $parsed")
        val formula = if (parsed.length > 31) parsed.substring(23, parsed.length - 8).replaceAllLiterally("\\n", "") else "No formula"
        val parsedFormula = FormulaParserInst.parse(formula).orElse {
          logger.debug(s"Couldn't parse: $formula")
          None
        }
        //    logger.debug(parsedFormula)
        Thread.sleep(50)

        if (count < 500) {
          Try {
            SageRequest.save(SageRequest(
              request = input,
              result = parsedFormula
            ))
          }
          cleanup()
        }

        parsedFormula
      } catch {
        case e: Exception =>
          logger.debug(s"Got exception $e")
          None
      }
    }
  }


  def oeisTerms(theory: String): Option[ArgList] = {
    val input = s"oeis('$theory').first_terms()"
    val result = callMethodWithCall(input)
    result.flatMap(argListToLong)
  }

  def generatingFunctionSeriesCoefficients(generatingFunction: Expression, terms: Int): Option[ArgList] = {
    val input =
      s"map(lambda xy: xy[0].simplify_full(), (${generatingFunction.toSagePython}).series(x,$terms).coefficients())"
    val result = callMethodWithCall(input)
    result.flatMap(argListToLong)
  }

  def oeisOffsets(theory: String): Option[ArgList] = {
    val input = s"oeis('$theory').offsets()"
    callMethodWithCall(input)
  }

  private def argListToLong(argList: ArgList): Option[ArgList] = {
    import parser._
    Try {
      val args = argList.args.map {
        case Neg(Add(List(Mul(List(Num(double), Var("e"))), Num(exp)))) => Neg(Num(s"${double}E${exp.toInt}".toDouble))
        case Add(List(Mul(List(Num(double), Var("e"))), Num(exp))) => Num(s"${double}E${exp.toInt}".toDouble)
        case x => x
      }
      println(" ARGLISTED " + args)
      ArgList(args)
    }.toOption
  }
}
