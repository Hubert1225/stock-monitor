package currencies

import java.time.Instant
import scala.util.{Try, Success, Failure}
import ujson.Value.Value
import upickle.core.LinkedHashMap

import utils._

type CurrencyCode = String

case class CurrenciesData(
    baseCurrency: CurrencyCode,
    rates: Map[CurrencyCode, Double],
    timeMoment: Instant
)

class CurrenciesDataRepository extends JsonApiHandler:
  private lazy val currenciesDataEndpoint = "https://open.er-api.com/v6/latest/USD"

  private def validateCurrencyDataMessage(
      messageObj: LinkedHashMap[String, ujson.Value.Value]
  ): Try[LinkedHashMap[String, ujson.Value.Value]] =
    if messageObj.contains("result") && messageObj("result").str == "success"
    then Success(messageObj)
    else
      Failure(
        new Exception(
          "The field \"result\" not present in the currencies data JSON or its value is not \"success\""
        )
      )

  private def currenciesDataToObject(
      currenciesData: LinkedHashMap[String, ujson.Value.Value]
  ): Try[CurrenciesData] = Try(
    CurrenciesData(
      baseCurrency = currenciesData("base_code").str,
      rates =
        (for (curCode, curRate) <- currenciesData("rates").obj.toMap yield (curCode, curRate.num)),
      timeMoment = Instant.ofEpochSecond(currenciesData("time_last_update_unix").num.toLong)
    )
  )

  def fetch(): Try[CurrenciesData] =
    requestGet(currenciesDataEndpoint)
      .flatMap((jsonContent: ujson.Value.Value) => Try(jsonContent.obj))
      .flatMap(validateCurrencyDataMessage)
      .flatMap(currenciesDataToObject)

def convertToBaseCurrency(value: Double, currentCurrency: CurrencyCode)(using
    currenciesData: CurrenciesData
): Double =
  value / currenciesData.rates(currentCurrency)

def convertFromBaseCurrency(value: Double, targetCurrency: CurrencyCode)(using
    currenciesData: CurrenciesData
): Double =
  value * currenciesData.rates(targetCurrency)

def convertCurrency(value: Double, currentCurrency: CurrencyCode, targetCurrency: CurrencyCode)(
    using currenciesData: CurrenciesData
): Double =
  val valueInBaseCurrency = convertToBaseCurrency(value, currentCurrency)
  convertFromBaseCurrency(valueInBaseCurrency, targetCurrency)
