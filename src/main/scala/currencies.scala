package currencies

import java.time.Instant
import scala.util.{Try, Success, Failure}
import ujson.Value.Value
import upickle.core.LinkedHashMap

import utils._

type currencyCode = String

case class CurrenciesData(
  baseCurrency: currencyCode,
  rates: Map[currencyCode, Double],
  timeMoment: Instant,
)

class CurrenciesDataRepository extends JsonApiHandler:
  private lazy val currenciesDataEndpoint = "https://open.er-api.com/v6/latest/USD"

  private def validateCurrencyDataMessage(message: ujson.Value.Value): Try[LinkedHashMap[String, ujson.Value.Value]] =
    val messageObjTry = Try(message.obj)
    messageObjTry match
      case Success(messageObj) =>
        if messageObj.contains("result") && messageObj("result").str == "success"
        then Success(messageObj)
        else Failure(
          new Exception("The field \"result\" not present in the currencies data JSON or its value is not \"success\"")
        )
      case Failure(exception) => messageObjTry

  private def currenciesDataToObject(currenciesData: LinkedHashMap[String, ujson.Value.Value]): Try[CurrenciesData] = Try(
    CurrenciesData(
      baseCurrency = currenciesData("base_code").str,
      rates = (for (curCode, curRate) <- currenciesData("rates").obj.toMap yield (curCode, curRate.num)),
      timeMoment = Instant.ofEpochSecond(currenciesData("time_last_update_unix").num.toLong)
    )
  )

  def fetch(): Try[CurrenciesData] =
    request_get(currenciesDataEndpoint)
    .flatMap(validateCurrencyDataMessage)
    .flatMap(currenciesDataToObject)
