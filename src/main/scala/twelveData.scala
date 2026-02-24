package twelveData

import scala.util.{Try, Success, Failure}
import ujson.Value.Value
import upickle.core.LinkedHashMap
import sttp.client4.*
import sttp.model.*

import utils._
import domain.{Stock, Exchange, TimeSeries, StockTimeSeries}
import java.time.{Instant, Duration}
import domain.TimeSeries

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
  lazy val timeSeriesEndpoint = endpointUrl("time_series")

  private def validateResponse(
      responseObj: LinkedHashMap[String, ujson.Value.Value]
  ): Try[LinkedHashMap[String, ujson.Value.Value]] =
    if responseObj.contains("status") && responseObj("status").str == "ok"
    then Success(responseObj)
    else
      Failure(
        new Exception("Response does not contain the \"status\" or status is unsuccessful")
      )

  private val datetimeStringPattern =
    "^([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})$".r
  private def datetimeStringToInstant(datetimeString: String): Instant =
    datetimeStringPattern.findFirstMatchIn(datetimeString) match
      case Some(datetimeMatch) =>
        val year = datetimeMatch.group(1)
        val month = datetimeMatch.group(2)
        val day = datetimeMatch.group(3)
        val hour = datetimeMatch.group(4)
        val minute = datetimeMatch.group(5)
        val second = datetimeMatch.group(6)
        Instant.parse(s"$year-$month-${day}T$hour:$minute:${second}Z")
      case None => throw new Exception(s"Could not parse datetime string: $datetimeString")

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

  private def parseTimeSeriesObject(
      stockSeriesJsonObj: LinkedHashMap[String, ujson.Value.Value]
  ): StockTimeSeries =
    val dataPoints = stockSeriesJsonObj("values").arr.reverse.toList
    val pointsInstants = for obj <- dataPoints yield datetimeStringToInstant(obj("datetime").str)

    if pointsInstants.length > 1 & !areInstantsEvenlySpaced(
        pointsInstants,
        Duration.between(pointsInstants(0), pointsInstants(1))
      )
    then throw new Exception("Found data points not evenly spaced in time")

    val seriesMap = List("open", "close", "high", "low")
      .map((seriesName) => seriesName -> (for obj <- dataPoints yield obj(seriesName).str.toDouble))
      .map((nameAndValues) =>
        nameAndValues._1 -> TimeSeries(
          values = nameAndValues._2.toVector,
          startTime = pointsInstants(0),
          interval = stockSeriesJsonObj("meta").obj("interval").str
        )
      )
      .toMap

    StockTimeSeries(
      stockSymbol = stockSeriesJsonObj("meta").obj("symbol").str,
      currency = stockSeriesJsonObj("meta").obj("currency").str,
      openValues = seriesMap("open"),
      closeValues = seriesMap("close"),
      highValues = seriesMap("high"),
      lowValues = seriesMap("low")
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
  val getTimeSeries =
    (queryParams: Option[Map[String, String]]) =>
      requestGet(timeSeriesEndpoint(queryParams))
        .map(_.obj)
        .flatMap(validateResponse)
        .map(parseTimeSeriesObject)
