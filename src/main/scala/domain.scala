package domain

import java.time.{Instant, Duration}

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

class TimeSeries(
    val values: Vector[Double],
    private val startTime: Instant,
    private val interval: String
):

  private val intervalPattern = "^([0-9]+)((min)|(h)|(days?))$".r

  private val intervalDuration = interval match
    case intervalPattern(n, unit, _, _, _) =>
      unit match
        case "min"  => Duration.ofMinutes(n.toInt)
        case "h"    => Duration.ofHours(n.toInt)
        case "day"  => Duration.ofDays(n.toInt)
        case "days" => Duration.ofDays(n.toInt)
        case _      => throw new Exception(s"Unknown time unit: $unit")
    case _ => throw new Exception(s"Given interval could not be parsed: $interval")

  def getStartTime = startTime
  def getInterval = interval
  lazy val length = values.length
  lazy val lastTime = startTime.plusSeconds(intervalDuration.toSeconds() * (length - 1))

  def getFromToIndex(fromIndex: Int, toIndex: Int): TimeSeries =
    require(fromIndex < toIndex)
    require(fromIndex >= 0)
    require(toIndex < length)
    val newValues = for index <- Range.inclusive(fromIndex, toIndex) yield values(index)
    TimeSeries(
      newValues.toVector,
      startTime.plusSeconds(intervalDuration.getSeconds() * fromIndex),
      interval
    )

  def getFromToTime(fromTime: Instant, toTime: Instant): TimeSeries =
    val startOffset =
      (Duration.between(startTime, fromTime).toSeconds().toDouble / intervalDuration
        .toSeconds()
        .toDouble).floor.toInt
    assert(startOffset >= 0)
    val endOffset = (Duration.between(toTime, lastTime).toSeconds().toDouble / intervalDuration
      .toSeconds()
      .toDouble).ceil.toInt
    assert(endOffset >= 0)
    getFromToIndex(startOffset, length - endOffset - 1)

case class StockTimeSeries(
    stockSymbol: String,
    currency: CurrencyCode,
    openValues: TimeSeries,
    closeValues: TimeSeries,
    highValues: TimeSeries,
    lowValues: TimeSeries
)
