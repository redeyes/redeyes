package redeyes.api.http

import redeyes.api._
import redeyes.parser._

/**
 * A module for describing parameters in query strings, e.g. q=search, limit=10,
 * etc.
 */
trait Queries extends ApiModule {
  import charParser._
  
  def queryInt(name: String): Api[BigInt]         = query(string(name), integer)
  
  def queryDecimal(name: String): Api[BigDecimal] = query(string(name), decimal)
  
  def queryString(name: String): Api[String]      = query(string(name), anyString)
}