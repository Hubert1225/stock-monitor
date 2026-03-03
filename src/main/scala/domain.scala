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
    TimeSeries(
      values.slice(fromIndex, toIndex + 1),  // inclusive on both sides
      startTime.plusSeconds(intervalDuration.getSeconds() * fromIndex),
      interval
    )

  /* Calculates how many time steps (intervals) from the start time
  to the given offset time is there */
  private def getOffset(offsetTime: Instant) =
    (Duration.between(startTime, offsetTime).toSeconds().toDouble / intervalDuration
      .toSeconds()
      .toDouble).floor.toInt

  def getFromToTime(fromTime: Instant, toTime: Instant): TimeSeries =
    require(fromTime.isBefore(toTime))
    val startOffset = getOffset(fromTime)
    require(startOffset >= 0)
    val endOffset = getOffset(toTime)
    require(endOffset >= 0)
    getFromToIndex(startOffset, endOffset)

case class StockTimeSeries(
    stockSymbol: String,
    currency: CurrencyCode,
    openValues: TimeSeries,
    closeValues: TimeSeries,
    highValues: TimeSeries,
    lowValues: TimeSeries
)
