import java.time.Instant
import currencies.*
class CurrenciesSuite extends munit.FunSuite {

  given CurrenciesData(
    baseCurrency = "USD",
    rates = Map(
        "FOK" -> 6.29, "BBD" -> 2.0, "PLN" -> 3.54, "FKP" -> 0.73, "MUR" -> 45.59, "XAF" -> 552.77,
        "CHF" -> 0.77, "EUR" -> 0.84, "CVE" -> 92.92, "USD" -> 1.0,
    ),
    timeMoment = Instant.now()
  )

  test("convertToBaseCurrency EUR -> USD") {
    val obtained = convertToBaseCurrency(100.0, "EUR")
    val expected = 119.04761904761905
    assertEquals(obtained = obtained, expected = expected)
  }

  test("convertToBaseCurrency CVE -> USD") {
    val obtained = convertToBaseCurrency(32.35, "CVE")
    val expected = 0.34814894532931556
    assertEquals(obtained = obtained, expected = expected)
  }

  test("convertFromBaseCurrency USD -> CHF") {
    val obtained = convertFromBaseCurrency(100.0, "CHF")
    val expected = 77.0
    assertEquals(obtained = obtained, expected = expected)
  }

  test("convertCurrency CHF -> BBD") {
    val obtained = convertCurrency(250.50, "CHF", "BBD")
    val expected = 650.6493506493506
    assertEquals(obtained = obtained, expected = expected)
  }
}
