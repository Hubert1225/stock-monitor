package twelveData

import scala.util.{Try, Success, Failure}
import ujson.Value.Value
import upickle.core.LinkedHashMap
import sttp.client4.*
import sttp.model.*

import utils._
import currencies.CurrencyCode

case class Exchange(
    title: String,
    name: String,
    code: String,
    country: String,
    timezone: String
)

case class Stock(
    symbol: String,
    name: String,
    currency: CurrencyCode,
    exchange: String,
    micCode: String,
    country: String,
    figiCode: String
)

class TwelveDataClient extends JsonApiHandler:

  private val apiKey = sys.env("TWELVE_DATA_API_KEY")
  private val endpointBase = "https://api.twelvedata.com/"
  private val defaultParams = Map("apikey" -> apiKey)

  private def endpointUrl(endpoint: String)(
      queryParams: Option[Map[String, String]] = None
  ): String =
    val resolvedParams = queryParams match
      case Some(params) => defaultParams ++ params
      case None         => defaultParams
    val endpointUri = uri"$endpointBase/$endpoint?$resolvedParams"
    endpointUri.toString

  lazy val exchangesEndpoint = endpointUrl("exchanges")
  lazy val stocksEndpoint = endpointUrl("stocks")

  private def validateResponse(
      responseObj: LinkedHashMap[String, ujson.Value.Value]
  ): Try[LinkedHashMap[String, ujson.Value.Value]] =
    if responseObj.contains("status") && responseObj("status").str == "ok"
    then Success(responseObj)
    else
      Failure(
        new Exception("Response does not contain the \"status\" or status is unsuccessful")
      )

  private def parseExchangeObject(
      exchangeJsonObj: LinkedHashMap[String, ujson.Value.Value]
  ): Exchange =
    Exchange(
      title = exchangeJsonObj("title").str,
      name = exchangeJsonObj("name").str,
      code = exchangeJsonObj("code").str,
      country = exchangeJsonObj("country").str,
      timezone = exchangeJsonObj("timezone").str
    )

  private def parseStockObject(stockJsonObj: LinkedHashMap[String, ujson.Value.Value]): Stock =
    Stock(
      symbol = stockJsonObj("symbol").str,
      name = stockJsonObj("name").str,
      currency = stockJsonObj("currency").str,
      exchange = stockJsonObj("exchange").str,
      micCode = stockJsonObj("mic_code").str,
      country = stockJsonObj("country").str,
      figiCode = stockJsonObj("figi_code").str
    )

  private def getObjectsFun[T](
      parseFun: (LinkedHashMap[String, ujson.Value.Value]) => T,
      endpoint: Option[Map[String, String]] => String
  ) =
    (queryParams: Option[Map[String, String]]) =>
      requestGet(endpoint(queryParams))
        .map(_.obj)
        .flatMap(validateResponse)
        .map(_("data").arr.toList)
        .map(_.map((jsonObj: ujson.Value.Value) => parseFun(jsonObj.obj)))

  val getExchanges = getObjectsFun(parseExchangeObject, exchangesEndpoint)
  val getStocks = getObjectsFun(parseStockObject, stocksEndpoint)
