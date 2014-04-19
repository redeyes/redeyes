package redeyes.api.http

import redeyes.api._
import redeyes.parser._

/**
 * A module that contains a more-or-less comprehensive collection of both common
 * (GET, PUT, POST, DELETE) and uncommon HTTP methods (MOVE, SEARCH, etc.).
 */
trait Methods extends ApiModule {
  import charParser._
  
  val GET:              Api[String] = method(string("GET"))
  val PUT:              Api[String] = method(string("PUT"))
  val POST:             Api[String] = method(string("POST"))
  val DELETE:           Api[String] = method(string("DELETE"))
  
  val OPTIONS:          Api[String] = method(string("OPTIONS"))
  val HEAD:             Api[String] = method(string("HEAD"))
  val TRACE:            Api[String] = method(string("TRACE"))
  val CONNECT:          Api[String] = method(string("CONNECT"))
  
  val MOVE:             Api[String] = method(string("MOVE"))
  val COPY:             Api[String] = method(string("COPY"))
  val PROPFIND:         Api[String] = method(string("PROPFIND"))
  val PROPPATCH:        Api[String] = method(string("PROPPATCH"))  
  val MKCOL:            Api[String] = method(string("MKCOL"))
  val LOCK:             Api[String] = method(string("LOCK"))
  val UNLOCK:           Api[String] = method(string("UNLOCK"))
  
  val VERSION_CONTROL:  Api[String] = method(string("VERSION-CONTROL"))
  val REPORT:           Api[String] = method(string("REPORT"))
  val CHECKOUT:         Api[String] = method(string("CHECKOUT"))
  val CHECKIN:          Api[String] = method(string("CHECKIN"))
  val UNCHECKOUT:       Api[String] = method(string("UNCHECKOUT"))
  val MKWORKSPACE:      Api[String] = method(string("MKWORKSPACE"))
  val UPDATE:           Api[String] = method(string("UPDATE"))
  val LABEL:            Api[String] = method(string("LABEL"))
  val MERGE:            Api[String] = method(string("MERGE"))
  val BASELINE_CONTROL: Api[String] = method(string("BASELINE-CONTROL"))
  val MKACTIVITY:       Api[String] = method(string("MKACTIVITY"))
  
  val ORDERPATCH:       Api[String] = method(string("ORDERPATCH"))
  
  val ACL:              Api[String] = method(string("ACL"))
  
  val PATCH:            Api[String] = method(string("PATCH"))
  
  val SEARCH:           Api[String] = method(string("SEARCH"))
  
  val POLL:             Api[String] = method(string("POLL"))
  val SUBSCRIBE:        Api[String] = method(string("SUBSCRIBE"))
  val UNSUBSCRIBE:      Api[String] = method(string("UNSUBSCRIBE"))
  val NOTIFY:           Api[String] = method(string("NOTIFY"))
}