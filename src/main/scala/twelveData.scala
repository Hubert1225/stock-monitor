package twelveData

import scala.util.{Try, Success, Failure}
import ujson.Value.Value
import upickle.core.LinkedHashMap
import sttp.client4.*
import sttp.model.*

import utils._
import domain.{Stock, Exchange, TimeSeries, StockTimeSeries}
import java.time.{Instant, Duration, LocalDateTime, ZoneOffset, ZoneId}
import java.time.format.DateTimeFormatter
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

  lazy val exchangesEndpointFactory = endpointUrl("exchanges")
  lazy val stocksEndpointFactory = endpointUrl("stocks")
  lazy val timeSeriesEndpointFactory = endpointUrl("time_series")

  private def validateResponse(
      responseObj: LinkedHashMap[String, ujson.Value.Value]
  ): Try[LinkedHashMap[String, ujson.Value.Value]] =
    if responseObj.contains("status") && responseObj("status").str == "ok"
    then Success(responseObj)
    else
      Failure(
        new Exception("Response does not contain the \"status\" or status is unsuccessful")
      )

  private def datetimeStringToInstant(datetimeString: String, exchangeTimezone: String): Instant =
    val exchangeZoneOffset = ZoneId.of(exchangeTimezone).getRules().getOffset(Instant.now())
    LocalDateTime
      .parse(datetimeString, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      .toInstant(exchangeZoneOffset)

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
    val exchangeTimezone = stockSeriesJsonObj("meta").obj("exchange_timezone").str
    val pointsInstants =
      for obj <- dataPoints yield datetimeStringToInstant(obj("datetime").str, exchangeTimezone)

    if pointsInstants.length > 1 && !areInstantsEvenlySpaced(
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
          intervalString = stockSeriesJsonObj("meta").obj("interval").str
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

  val getExchanges = getObjectsFun(parseExchangeObject, exchangesEndpointFactory)
  val getStocks = getObjectsFun(parseStockObject, stocksEndpointFactory)
  val getTimeSeries =
    (queryParams: Option[Map[String, String]]) =>
      requestGet(timeSeriesEndpointFactory(queryParams))
        .map(_.obj)
        .flatMap(validateResponse)
        .map(parseTimeSeriesObject)
