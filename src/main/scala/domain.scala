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

enum TimeInterval:
  case Minutes(n: Int)
  case Hours(n: Int)
  case Days(n: Int)

  def toDuration: Duration = this match
    case Minutes(n) => Duration.ofMinutes(n)
    case Hours(n)   => Duration.ofHours(n)
    case Days(n)    => Duration.ofDays(n)

class TimeSeries(
    val values: Vector[Double],
    val startTime: Instant,
    intervalString: String
):

  // resolve interval
  
  private val intervalPattern = "^([0-9]+)(min|h|days?)$".r

  val interval = intervalString match
    case intervalPattern(n, unit) =>
      unit match
        case "min"  => TimeInterval.Minutes(n.toInt)
        case "h"    => TimeInterval.Hours(n.toInt)
        case "day"  => TimeInterval.Days(n.toInt)
        case "days" => TimeInterval.Days(n.toInt)
        case _      => throw new Exception(s"Unknown time unit: $unit")
    case _ => throw new Exception(s"Given interval could not be parsed: $intervalString")

  lazy val intervalDuration = interval.toDuration

  // series length and last time point

  lazy val length = values.length
  lazy val lastTime = startTime.plusSeconds(intervalDuration.toSeconds() * (length - 1))

  // slicing methods

  def getFromToIndex(fromIndex: Int, toIndex: Int): TimeSeries =
    require(fromIndex < toIndex)
    require(fromIndex >= 0)
    require(toIndex < length)
    TimeSeries(
      values.slice(fromIndex, toIndex + 1),  // inclusive on both sides
      startTime.plusSeconds(intervalDuration.getSeconds() * fromIndex),
      intervalString
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
