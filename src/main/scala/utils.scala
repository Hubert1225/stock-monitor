package utils

import scala.util.{Try, Success, Failure}
import sttp.client4.quick._
import sttp.client4.Response
import ujson.read
import ujson.Value.Value

/** A utility for convenient interaction with Web APIs sending JSON data
  */
trait JsonApiHandler:
  private def tryParseJson(body: String): Try[ujson.Value.Value] =
    Try(ujson.read(body))

  def requestGet(url: String): Try[ujson.Value.Value] =
    Try(quickRequest.get(uri"$url").send())
      .flatMap((response: Response[String]) => tryParseJson(response.body))
