package library

import parser.Line
import processor.TextParserIns

import cats.effect._
import com.comcast.ip4s._
import io.circe._
import io.circe.generic.auto._
import io.circe.literal._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.ember.server._
import org.http4s.implicits._
import scala.concurrent.duration._

case class Hello(greeting: String)
case class User(name: String)
case class OeisLineRequest(content: String)
case class OeisLineResponse(content: Option[Line])
// case class OeisLine(record: String, line: Int, text: String)


object Service extends IOApp {
  implicit val decoder = jsonOf[IO, User]
  implicit val oeisLineRequestDecoder = jsonOf[IO, OeisLineRequest]
  implicit val oeisLineResponseEncoder: Encoder[OeisLineResponse] =
    Encoder.instance {(oeis_response: OeisLineResponse) =>
        json"""${oeis_response.content}"""
    }
  implicit val optionLineEncoder: Encoder[Option[Line]] =
    Encoder.instance {(option_line: Option[Line]) =>
      println(option_line)
      option_line match {
        case None => json"""null"""
        case Some(x) => json"""${x}"""
      }
    }
  implicit val lineEncoder: Encoder[Line] =
    Encoder.instance {(line: Line) =>
      json"""${line.toString}"""
    }

  def run(args: List[String]): IO[ExitCode] =
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(service)
      .build
      .use(_ => IO.never)
      .as(ExitCode.Success)

  val service = HttpRoutes.of[IO] {
    case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")
    case req @ POST -> Root / "hello" =>
      for {
        user <- req.as[User]
        resp <- Ok(Hello(user.name).asJson)
      } yield (resp)
    case req @ POST -> Root / "parse_oeis" =>
      println(req)
      for {
        oeis_raw <- req.as[OeisLineRequest]
        // oeis_line <- TextParserIns.parseLine(oeis_raw.content).get
        // resp <- Ok(OeisLineResponse(oeis_line).asJson)
        // theory_representation <- TextParserIns.parseLine(oeis_raw.content)
        resp <- Ok(OeisLineResponse(None).asJson)
      } yield(resp)
  }.orNotFound
}
