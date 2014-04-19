package redeyes.api.http

import redeyes.api._
import redeyes.parser._

import scalaz.std.list._
import scalaz.std.string._


/**
 * This module describes segments of paths that appear in a request.
 */
trait Paths extends ApiModule {
  import charParser._
  
  /**
   * Describes a prefix of a path -- that is, a path possibly followed by something else.
   *
   * path("/foo/bar/baz")
   */
  def path(value: String): Api[String] = path(string(value))
  
  /**
   * Describes a path followed by the end of input.
   */
  def path$(value: String): Api[String] = path(string(value) <~< char('/').maybe <~< charEnd)
  
  /**
   * Describes an numeric identifier in the path.
   *
   * {{{
   * // /stores/123
   * path("/stores/") >~> pathId
   * }}}
   */
  val pathId: Api[BigInt] = path(integer)
  
  /**
   * Describes anything but slashes in a path element. Requires at least one character.
   */
  val pathElement: Api[String] = path(satisfy(c => c != '/').some.string)
  
  /**
   * Describes the remainder of the path.
   */
  val remainingPath: Api[String] = path(anyString <~< charEnd)
}