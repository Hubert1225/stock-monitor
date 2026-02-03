package twelveData

import scala.util.{Try, Success, Failure}
import ujson.Value.Value
import upickle.core.LinkedHashMap

import utils._

case class Exchange(
    title: String,
    name: String,
    code: String,
    country: String,
    timezone: String
)

class TwelveDataClient extends JsonApiHandler:

  private val apiKey = sys.env("TWELVE_DATA_API_KEY")

  lazy val endpointBase = "https://api.twelvedata.com/"

  private def endpointUrl(endpoint: String): String =
    endpointBase.concat(endpoint).concat(s"?apikey=$apiKey")

  lazy val exchangesEndpoint = endpointUrl("exchanges")

  private def validateResponse(
      responseObj: LinkedHashMap[String, ujson.Value.Value]
  ): Try[LinkedHashMap[String, ujson.Value.Value]] =
    if responseObj.contains("status") && responseObj("status").str == "ok"
    then Success(responseObj)
    else
      Failure(
        new Exception("Response does not contain the \"status\" or status is unsuccessful")
      )

  def parseExchangeObject(exchangeJsonObj: LinkedHashMap[String, ujson.Value.Value]): Exchange =
    Exchange(
      title = exchangeJsonObj("title").str,
      name = exchangeJsonObj("name").str,
      code = exchangeJsonObj("code").str,
      country = exchangeJsonObj("country").str,
      timezone = exchangeJsonObj("timezone").str
    )

  def getExchanges(): Try[List[Exchange]] =
    requestGet(exchangesEndpoint)
      .map(_.obj)
      .flatMap(validateResponse)
      .map(_("data").arr.toList)
      .map(_.map((exchangeJsonObj: ujson.Value.Value) => parseExchangeObject(exchangeJsonObj.obj)))
