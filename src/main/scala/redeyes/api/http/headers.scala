package redeyes.api.http

import redeyes.parser._
import redeyes.api._
import redeyes.bytes._

case class Header(name: String, value: String) extends Prerendered {
  val prerendered = (name.getBytes("US-ASCII") ++ value.getBytes("US-ASCII")).asInstanceOf[ByteArray]
}

trait RequestHeaders extends ApiModule {
  import charParser._
  
  private def mkHeader(name: String, value: String): Api[Header] = {
    val h = Header(name, value)

    val const: String <=> Header = Equiv[String, Header](
      to   = s => h,
      from = h => value
    )
    
    header(stringCI(name), stringCI(value)).map(Equiv.const(value, h))
  }
  private def mkHeader[A](name: String, value: CParser[A]): Api[A] = {
    header(stringCI(name), value)
  }
  
  def accept(value: String): Api[Header]              = mkHeader("Accept", value)
  def accept[A](value: CParser[A]): Api[A]            = mkHeader("Accept", value)
  def acceptCharset(value: String): Api[Header]       = mkHeader("Accept-Charset", value)
  def acceptCharset[A](value: CParser[A]): Api[A]     = mkHeader("Accept-Charset", value)
  def acceptEncoding(value: String): Api[Header]      = mkHeader("Accept-Encoding", value)
  def acceptEncoding[A](value: CParser[A]): Api[A]    = mkHeader("Accept-Encoding", value)
  def acceptLanguage(value: String): Api[Header]      = mkHeader("Accept-Language", value)
  def acceptLanguage[A](value: CParser[A]): Api[A]    = mkHeader("Accept-Language", value)
  def acceptDatetime(value: String): Api[Header]      = mkHeader("Accept-Datetime", value)
  def acceptDatetime[A](value: CParser[A]): Api[A]    = mkHeader("Accept-Datetime", value)
  def authorization(value: String): Api[Header]       = mkHeader("Authorization", value)
  def authorization[A](value: CParser[A]): Api[A]     = mkHeader("Authorization", value)
  def cacheControl(value: String): Api[Header]        = mkHeader("Cache-Control", value)
  def cacheControl[A](value: CParser[A]): Api[A]      = mkHeader("Cache-Control", value)
  def connection(value: String): Api[Header]          = mkHeader("Connection", value)
  def connection[A](value: CParser[A]): Api[A]        = mkHeader("Connection", value)
  def cookie(value: String): Api[Header]              = mkHeader("Cookie", value)
  def cookie[A](value: CParser[A]): Api[A]            = mkHeader("Cookie", value)
  def contentLength(value: String): Api[Header]       = mkHeader("Content-Length", value)
  def contentLength[A](value: CParser[A]): Api[A]     = mkHeader("Content-Length", value)
  def contentMD5(value: String): Api[Header]          = mkHeader("Content-MD5", value)
  def contentMD5[A](value: CParser[A]): Api[A]        = mkHeader("Content-MD5", value)
  def contentType(value: String): Api[Header]         = mkHeader("Content-Type", value)
  def contentType[A](value: CParser[A]): Api[A]       = mkHeader("Content-Type", value)
  def date(value: String): Api[Header]                = mkHeader("Date", value)
  def date[A](value: CParser[A]): Api[A]              = mkHeader("Date", value)
  def expect(value: String): Api[Header]              = mkHeader("Expect", value)
  def expect[A](value: CParser[A]): Api[A]            = mkHeader("Expect", value)
  def from(value: String): Api[Header]                = mkHeader("From", value)
  def from[A](value: CParser[A]): Api[A]              = mkHeader("From", value)
  def host(value: String): Api[Header]                = mkHeader("Host", value)
  def host[A](value: CParser[A]): Api[A]              = mkHeader("Host", value)
  def ifMatch(value: String): Api[Header]             = mkHeader("If-Match", value)
  def ifMatch[A](value: CParser[A]): Api[A]           = mkHeader("If-Match", value)
  def ifModifiedSince(value: String): Api[Header]     = mkHeader("If-Modified-Since", value)
  def ifModifiedSince[A](value: CParser[A]): Api[A]   = mkHeader("If-Modified-Since", value)
  def ifNoneMatch(value: String): Api[Header]         = mkHeader("If-None-Match", value)
  def ifNoneMatch[A](value: CParser[A]): Api[A]       = mkHeader("If-None-Match", value)
  def ifRange(value: String): Api[Header]             = mkHeader("If-Range", value)
  def ifRange[A](value: CParser[A]): Api[A]           = mkHeader("If-Range", value)
  def ifUnmodifiedSince(value: String): Api[Header]   = mkHeader("If-Unmodified-Since", value)
  def ifUnmodifiedSince[A](value: CParser[A]): Api[A] = mkHeader("If-Unmodified-Since", value)
  def maxForwards(value: String): Api[Header]         = mkHeader("Max-Forwards", value)
  def maxForwards[A](value: CParser[A]): Api[A]       = mkHeader("Max-Forwards", value)
  def origin(value: String): Api[Header]              = mkHeader("Origin", value)
  def origin[A](value: CParser[A]): Api[A]            = mkHeader("Origin", value)
  def pragma(value: String): Api[Header]              = mkHeader("Pragma", value)
  def pragma[A](value: CParser[A]): Api[A]            = mkHeader("Pragma", value)
  def proxyAuthorization(value: String): Api[Header]  = mkHeader("Proxy-Authorization", value)
  def proxyAuthorization[A](value: CParser[A]): Api[A]= mkHeader("Proxy-Authorization", value)
  def range(value: String): Api[Header]               = mkHeader("Range", value)
  def range[A](value: CParser[A]): Api[A]             = mkHeader("Range", value)
  def referer(value: String): Api[Header]             = mkHeader("Referer", value)
  def referer[A](value: CParser[A]): Api[A]           = mkHeader("Referer", value)
  def tE(value: String): Api[Header]                  = mkHeader("TE", value)
  def tE[A](value: CParser[A]): Api[A]                = mkHeader("TE", value)
  def upgrade(value: String): Api[Header]             = mkHeader("Upgrade", value)
  def upgrade[A](value: CParser[A]): Api[A]           = mkHeader("Upgrade", value)
  def userAgent(value: String): Api[Header]           = mkHeader("User-Agent", value)
  def userAgent[A](value: CParser[A]): Api[A]         = mkHeader("User-Agent", value)
  def via(value: String): Api[Header]                 = mkHeader("Via", value)
  def via[A](value: CParser[A]): Api[A]               = mkHeader("Via", value)
  def warning(value: String): Api[Header]             = mkHeader("Warning", value)
  def warning[A](value: CParser[A]): Api[A]           = mkHeader("Warning", value)
}

trait ResponseHeaders extends ApiModule {
  import charParser._

  // TODO:
}