package utils

import sttp.client4.quick._
import sttp.client4.Response
import ujson.read
import ujson.Value.Value

/**
 * A utility for convenient interaction with Web APIs sending
 * JSON data 
 */
trait JsonApiHandler:
  def request_get(url: String): ujson.Value.Value =
    val response = quickRequest.get(uri"$url").send()
    ujson.read(response.body)
