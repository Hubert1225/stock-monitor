package domain

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
