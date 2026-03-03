package utils

import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec
import sttp.client4.quick._
import sttp.client4.Response
import ujson.read
import ujson.Value.Value
import java.time.{Instant, Duration}

/** A utility for convenient interaction with Web APIs sending JSON data
  */
trait JsonApiHandler:
  private def tryParseJson(body: String): Try[ujson.Value.Value] =
    Try(ujson.read(body))

  def requestGet(url: String): Try[ujson.Value.Value] =
    Try(quickRequest.get(uri"$url").send())
      .flatMap((response: Response[String]) => tryParseJson(response.body))

/** Checks whether all breaks between time moments indicated by subsequent `Instant` instances are
  * equal to `expectedBreak`
  */
@tailrec
def areInstantsEvenlySpaced(instants: List[Instant], expectedBreak: Duration): Boolean =
  instants match
    case instantPrev :: instantNext :: rest =>
      if Duration.between(instantPrev, instantNext) == expectedBreak
      then areInstantsEvenlySpaced(instants.tail, expectedBreak)
      else false
    case _ => true
