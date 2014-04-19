package redeyes.api

import redeyes.doc._

import scalaz._

import Id._

import scalaz.std.option._

/**
 * The service module describes services. This is the core abstraction for
 * micro-services.
 */
trait ServiceModule extends ApiModule {
  /**
   * Monadic services. A service consists of a description of the input, 
   * a description of the output, and a function that can map from the
   * input to the monadic output.
   */
  case class ServiceM[M[_], A, B](in: Api[A], out: Api[B], f: A => M[B], doc: Option[Doc] = None) {
    /**
     * Adds documentation to the service. This is separate from documentation for
     * either the input or the output.
     */
    def +? [C: Documented](c: C) = copy(doc = Semigroup[Option[Doc]].append(doc, Some(Documented[C].document(c))))
  }

  type Service[A, B] = ServiceM[Id, A, B]

  /**
   * Constructs a non-monadic service from a description of the request and response and
   * a handler for the request type.
   * {{{
   * val myService = service(request, response) { (request: Request) =>
   *   doWithRequest(request)
   * }
   * }}}
   */
  def service[A, B](in: Api[A], out: Api[B]) = serviceM[Id, A, B](in, out)

  /**
   * Constructs a monadic service from a description of the request and response and
   * a handler for the request type.
   * {{{
   * val myService = serviceM(request, response) { (request: Request) =>
   *   doWithRequest(request)
   * }
   * }}}
   */
  def serviceM[M[_], A, B](in: Api[A], out: Api[B]) = (f: A => M[B]) => ServiceM(in, out, f)
}