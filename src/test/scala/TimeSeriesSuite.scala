import java.time.Instant
import domain.TimeSeries

class TimeSeriesSuite extends munit.FunSuite {
    test("correct lastTime") {
        val ts = TimeSeries(
            values = Vector(1.2, 0.0, -0.123, 0.5, 0.6, 123.123456789),
            startTime = Instant.parse("2026-03-12T11:00:00Z"),
            intervalString = "15min"
        )
        val expectedLastTime = Instant.parse("2026-03-12T12:15:00Z")
        assertEquals(obtained = ts.lastTime, expected = expectedLastTime)
    }

    test("correct slice by index 1") {
        val ts = TimeSeries(
            values = Vector(1.2, 0.0, -0.123, 0.5, 0.6, 123.123456789),
            startTime = Instant.parse("2026-03-12T11:00:00Z"),
            intervalString = "15min"
        )
        val sliced = ts.getFromToIndex(0, 3)
        assertEquals(
            obtained = sliced.values,
            expected = Vector(1.2, 0.0, -0.123, 0.5)
        )
        assertEquals(
            obtained = sliced.length,
            expected = 4
        )
        assertEquals(
            obtained = sliced.startTime,
            expected = Instant.parse("2026-03-12T11:00:00Z")
        )
    }

    test("correct slice by index 2") {
        val ts = TimeSeries(
            values = Vector(1.2, 0.0, -0.123, 0.5, 0.6, 123.123456789),
            startTime = Instant.parse("2026-03-12T11:00:00Z"),
            intervalString = "1h"
        )
        val sliced = ts.getFromToIndex(2, 4)
        assertEquals(
            obtained = sliced.values,
            expected = Vector(-0.123, 0.5, 0.6)
        )
        assertEquals(
            obtained = sliced.length,
            expected = 3
        )
        assertEquals(
            obtained = sliced.startTime,
            expected = Instant.parse("2026-03-12T13:00:00Z")
        )
    }

    test("correct slice by time 1") {
        val ts = TimeSeries(
            values = Vector(-123.456, 12.5, -5041.789, 0.00001, 0.61, 74.09, 61.3, 4.0, 99.999999999, 14.2, -76.95625),
            startTime = Instant.parse("2025-11-23T01:00:00Z"),
            intervalString = "10min"
        )
        val sliced = ts.getFromToTime(
            fromTime = Instant.parse("2025-11-23T01:40:00Z"),
            toTime = Instant.parse("2025-11-23T02:30:00Z"),
        )
        assertEquals(
            obtained = sliced.values,
            expected = Vector(0.61, 74.09, 61.3, 4.0, 99.999999999, 14.2)
        )
        assertEquals(
            obtained = sliced.startTime,
            expected = Instant.parse("2025-11-23T01:40:00Z")
        )
        assertEquals(
            obtained = sliced.lastTime,
            expected = Instant.parse("2025-11-23T02:30:00Z")
        )
    }

    test("correct slice by time 2") {
        val ts = TimeSeries(
            values = Vector(-123.456, 12.5, -5041.789, 0.00001, 0.61, 74.09, 61.3, 4.0, 99.999999999, 14.2, -76.95625),
            startTime = Instant.parse("2025-11-23T01:00:00Z"),
            intervalString = "10min"
        )
        val sliced = ts.getFromToTime(
            fromTime = Instant.parse("2025-11-23T01:40:00Z"),
            toTime = Instant.parse("2025-11-23T02:13:45Z"),
        )
        assertEquals(
            obtained = sliced.values,
            expected = Vector(0.61, 74.09, 61.3, 4.0)
        )
        assertEquals(
            obtained = sliced.startTime,
            expected = Instant.parse("2025-11-23T01:40:00Z")
        )
        assertEquals(
            obtained = sliced.lastTime,
            expected = Instant.parse("2025-11-23T02:10:00Z")
        )
    }
}
