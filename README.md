# Introduction

This is an early, experimental preview release of RedEyes.

RedEyes is the spiritual successor to [BlueEyes](http://github.com/jdegoes/blueeyes/).

RedEyes is intended to be laser-focused on the following goal:

 * To provide a type-safe, composable, boilerplate- and macro-free way of creating external-facing HTTP services that must be well-documented and consumed by a variety of programming languages.

Eventually, RedEyes might be generalized to handle other text-oriented protocols.

# Preview

The goal is to support the following syntax and set of features:

```scala
import redeyes.api._

val request: Api[Int] = 
  GET >~>
  path("/employees/") >~>
  contentType("application/json") >~>
  queryInt("limit")

val response: Api[Json] = 
  contentType("application/json") >~> content(JsonCodec)

val listEmployees = serviceM(request, response) { limit =>
  loadAllEmployees(limit).toJson
}

val docs: Markdown = document(listEmployees)

val server: Task[Unit] = compileToServer(listEmployees)

val remoteService: Int => Task[Json] = remotely(listEmployees)("localhost", 80)

server.run
```

# Status

RedEyes has an API for constructing services, but does not yet have a compiler to a specific server backend. The current plan is to produce a native backend for scalaz-stream over raw NIO. 

Many tests are missing and some of the interior surface area (especially the existing encoding of GADTs and the use of modules to reuse code across Char and Api parsers) could be greatly improved.

Pull requests welcome!

# Quick Start

TODO

# History

This detailed history of the development of RedEyes is supplementary material for the talk *Building Web Services with RedEyes & Scala*, at LambdaConf 2014. Others might find it useful as a way of understanding the major concepts in RedEyes.

## BlueEyes

October 12, 2010. The date of the first commit to the [BlueEyes repository](http://github.com/jdegoes/blueeyes).

I was just discovering functional programming.

In fact, 4 years later, I am *still* discovering functional programming!

Fortunately, even back then I had picked up some good habits over the years, including preferring immutable data structures, eschewing void-accepting and void-returning functions, mandating all `if` statements have corresponding `else` branches, and so on.

At the time, I was working for SocialMedia.com, a rich media adtech company with an API-driven ad server. 

We had a *lot* of web APIs to build.

I disliked all the web frameworks I knew about, because they made life complicated. They were very stateful and mutable, making them hard to reason about. They assumed you wanted to print HTML on the server-side. And most used dynamic typing in some part of the stack (e.g. templates), which meant a lot of things could go wrong at runtime.

For my day job, I wanted a super simple, high-performance means of *rapidly* and *safely* creating HTTP services.

Out of that desire, BlueEyes was born.

## Wheels of Stone

The core idea behind BlueEyes is that a web service is "just" a function from a request to a response. Or for an asynchronous web service, you might say a function from a request to a `Future` of response.

The earliest versions of BlueEyes used a `type` synonym to describe this notion:

```scala
type Handler[T] = (Map[Symbol, String], HttpRequest[T]) => Future[HttpResponse[T]]
```

Though the exact form changed over time, this kernel of an idea (and sometimes even the BlueEyes composition syntax!) has been replicated in many other Scala frameworks (Spray, Finagle, Http4s, etc.).

Initially, I didn't see the compositional properties in *service handlers as functions*. 

So to build services, you used a godawful `RestHierarchyBuilder`:

```scala
trait RestHierarchyBuilder[T] extends RestHierarchy[T] {
  import scala.collection.mutable.{Stack, ArrayBuffer}
  
  private type Handler[T] = (Map[Symbol, String], HttpRequest[T]) => Future[HttpResponse[T]]
  
  private val pathStack: Stack[RestPathPattern] = new Stack[RestPathPattern].push(RestPathPattern.Root);
  private val _hierarchy: ArrayBuffer[(RestPathPattern, HttpMethod, Handler[T])] = new ArrayBuffer
  
  def hierarchy = _hierarchy.toList
  
  def path(path: RestPathPattern)(f: => Unit): Unit = {
    pathStack.push(path)
    
    try { f } finally { pathStack.pop() }
  }
  
  def get(handler: Handler[T]) = custom(GET, handler)
  
  def put(handler: Handler[T]) = custom(PUT, handler)
  
  def post(handler: Handler[T]) = custom(POST, handler)
  
  def delete(handler: Handler[T]) = custom(DELETE, handler)
  
  def options(handler: Handler[T]) = custom(OPTIONS, handler)

  ...
}
```

But within a couple months, I [discovered](https://github.com/jdegoes/blueeyes/commit/d135c8bdeeca923a2c2cee92a97457ddb2ae9f91) that function composition is sufficiently powerful to allow the construction of services in a highly modular way.

In a [quick spike](https://github.com/jdegoes/blueeyes/commit/d135c8bdeeca923a2c2cee92a97457ddb2ae9f91), I sketched out what composition could look like with a streamlined definition of request handlers:

```scala
type Handler[T, S] = HttpRequest[T] => Future[HttpResponse[S]]
...
post {
  require[JObject] {
    require(!(_ \ "adId" -->? classOf[JString]).isEmpty) { request =>
      val adId = (request.content \ "adId").deserialize[String]
    }
  }
}
```

Over the days that followed, I *leveled-up*, as they say, and found out you could write a huge collection of combinators that allow you to create just about any HTTP REST API you want.

Combinators to match paths. Combinators to match query string parameters. Combinators to match HTTP method types. Combinators to decode requests and encode responses. Combinators for alternation (try one handler, then try another handler).

Combinators, combinators, combinators!

Here's a simple example taken from Noel Welsh's [online book](http://noelwelsh.com/blueeyes/intro.html) on BlueEyes:

```scala
path("/add" / 'number1 / 'number2) {
  (request: HttpRequest[ByteChunk]) =>
    try {
      val number1 = request.parameters('number1).toInt
      val number2 = request.parameters('number2).toInt
      val sum = (number1 + number2).toString

      Future {
        HttpResponse[ByteChunk](content = Some(sum.toString))
      }
    } catch {
        case e: NumberFormatException =>
          Future {
            HttpResponse[ByteChunk](status = HttpStatus(BadRequest))
          }
    }
} ~
path("/multiply" / 'number1 / 'number2) {
  (request: HttpRequest[ByteChunk]) =>
    try {
      val number1 = request.parameters('number1).toInt
      val number2 = request.parameters('number2).toInt
      val product = (number1 * number2).toString

      Future {
        HttpResponse[ByteChunk](content = Some(product.toString))
      }
    } catch {
        case e: NumberFormatException =>
          Future {
            HttpResponse[ByteChunk](status = HttpStatus(BadRequest))
          }
    }
}
```

For everything I got wrong &mdash; and trust me, I got a lot wrong! &mdash; I had discovered the compositional power of functional programming, and it was sweet nectar of the gods.

## The Dirty on Function Composition

Functions compose beautifully, but they have a darker side: they are black boxes.

I first discovered this when I tried to implement automatic support for the HTTP method `OPTION`.

A quick refresher for those of you who aren't HTTP buffs: `OPTION` is supposed to tell you what forms of communication are supported at a given path.

Since a user of BlueEyes already specifies what forms of communication are supported &mdash; by supporting them! &mdash; I wanted a boilerplate-free way of supporting the `OPTION` method.

Unfortunately, that's just not possible.

A web service is just a function, and when you compose two functions, the result is another function. All information on the *derivation* of a function is sucked into a black hole (unless you dive into the unsafe, murky world of reflection, which some frameworks do!).

Semantics are gone, and all that's left is the function, whose type can tell you the domain and codomain, but nothing else.

This loss of information turns out to have *rippling effects* downstream.

### Internal Code

Without semantic information, users are forced to implement things like `OPTION` themselves, which means the code contains duplication.

Not duplication of code, per se, which is easily factored out, but duplication of *information* &mdash; which is a less-recognized, but no less egregious form of duplication.

This happens anywhere the HTTP standard implies that supporting one thing also implies supporting something else.

### Documentation

Most web services are consumed by the public, or at least by engineers who didn't develop them. They're consumed in a variety of programming languages, ranging from Javascript front-ends to Haskell back-ends. 

You really can't choose what languages or technologies the consumers of your API use.

As a result, the most successful APIs are ones that have been *well-documented*.

Unfortunately, documenting a service also involves its own form of duplication.

Let's say you write a handler to support HTTP GET with a particular path, which requires some content type and produces another.

If you want to document this service, you'll have to replicate the exact same information in English! Why should you have to do that, when the information is already encoded into your program?

Indeed, documenting APIs, and keeping that documentation accurate and in sync with the code base, remains one of the biggest pains of developing and maintaining web APIs.

It was a big pain at SocialMedia.com, the company where I initially developed BlueEyes, and it was an even bigger pain at Precog, the next company I started. Countless weeks were spent writing and re-writing documentation when we changed the API, and yet more time was spent helping customers track down problems caused by missing or inaccurate documentation.

### Client Libraries

Sometimes, all you need are well-documented web APIs. But other times, the barrier of entry to using those APIs is still too high, and to lower that barrier, you end up needing to create *client libraries*.

Client libraries wrap the web API and provide language-specific, idiomatic interfaces to the same functionality.

Like documentation, they are extremely painful to develop and keep up to date. You often need to create 4 - 6 different client libraries, one for every mainstream programming language (plus 1 for the niche functional language that you use!).

It takes at least 2 weeks to develop a minimal client library for a relatively straightforward API. Multiply that by the number of languages, and then include the cost of keeping all client libraries in sync with the API, and we are taking about a huge amount of effort.

As with documentation, though, your program already encodes what your web API expects. Wouldn't it be nice if you could somehow use this information to avoid manually developing client libraries?

## A Short Fling with Metadata

For a while, [Kris Nuttycombe](http://twitter.com/nuttycom) and I thought the answer to this problem might lie in metadata.

Indeed, we modified BlueEyes so that the core request handler was more than just a function &mdash; you could attach metadata to it:

```scala
sealed trait AnyService {
  def metadata: Metadata
  ...
}

sealed trait HttpService[A, B] extends AnyService { self =>
  def service: HttpRequest[A] => Validation[NotServed, B]

  ...

  def withMetadata(m: Metadata) = new MetadataService(m, this)
}
```

Metadata could contain human-readable text that could be converted into documentation. Indeed, one service combinator added the ability for a service to document itself, using built-in metadata!

This approach was a definite improvement, but still suffered from a number of limitations:

 1. Developers still had to write documentation, albeit not for everything (built-in services shipped with documentation) and they could keep their documentation alongside the code to make it easier to keep in sync.
 2. It didn't address duplication in request handlers or client libraries.

I wanted a better abstraction. Something that allowed developers to *describe* their services one time, and then based on that *description*, generate a server, documentation, client libraries, and so forth.

A sort of *cross-compilation* of *web service descriptors* to multiple targets, if you will.

I needed an abstraction to allow me to describe web services.

But what that abstraction looked like remained a complete mystery for a long time.

## The Long Winter

For two long years, BlueEyes remained essentially unchanged. Various improvements were made to content handling, bugs were fixed, and combinators added &mdash; but the core API didn't change much.

Meanwhile, I wasn't coding anymore, and my rate of learning slowed to a crawl. I spent most of my time in meetings, email, and docs.

But I did think about BlueEyes, and about possible solutions to the problem of duplicated information.

In June 2013, my company Precog was acquired by RichRelevance. I stuck around for a few months, but decided I didn't want to work there, so I left and joined the ranks of independent consultants.

A hired gun for anything from architecture to implementation.

After 2 years of stagnation, I finally had a chance to code again. To read blogs and papers. To dig into and study good libraries. To level-up as a functional programmer.

### Breakthrough

Some time in early December 2013, I read in a blog post mentioning that applicative functors were suited for "static analysis", while monads were not.

Even though not understanding exactly how, I thought static analysis of "web service descriptors" might be a way to generate multiple things from the same description: request handlers, documentation, maybe even client libraries.

*That* caught my attention.

## Applicative Functors to the Rescue

Every monad forms an applicative functor, but most libraries that implement an applicative interface also implement a monadic one.

As a result, I always used monadic interfaces, and never closely examined the differences between the two.

But reading that blog post made me stop and think. I immediately dug into the `Applicative` functor type class.

The most relevant methods are shown below:

```scala
def map[A, B](fa: F[A])(f: A => B): F[B]

def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B]

def point[A](a: => A): F[A]

```

Compare this definition to the `Monad` type class, which inherits all of the above but adds the following functionality:

```scala
def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
```

Specifically, compare the function `ap` to the function `bind`.

These functions are obviously similar, and in fact they're often used for the same purpose.

Both `bind` and `ap` accept a functor (which is monadic or applicative, respectively), as well as a function that uses a value inside the functor to produce a value of a different type.

Essentially, `bind` and `ap` are "transformation" functions. They transform the value inside the functor to a value of another type. 

The differences in the types tell you *everything* you need to know:

> **When you call `bind`, your function can decide at "runtime" which `F[B]` to produce based on the value of `A`. However, when you call `ap`, you have already decided on both `F[A]` and `F[A => B]`. Your only "runtime" decision is which `B` you want to produce given an `A`.**

Thus, monads allow *context-sensitive* transformations, while merely applicative functors deny them.

This is what makes monads so ungodly powerful. But with great power comes, well: a black box that you can't peer into.

With `Applicative` functors, you can peek into the box!

### Peering into Applicative Functors

To demonstrate what I mean, let's look at `Option`. `Option` is one of the simplest possible `Applicative` functors.

If we're writing the implementation of `ap`, we know whether `F[A]` is `Some[A]` or `None`, and we know whether `F[A => B]` is `Some[A => B]` or `None`. In fact, here's some code to discriminate on all the possibilities:

```scala
def ap[A](fa: Option[A])(f: Option[A => B]): Option[B] = (fa, f) match {
  case (Some(a), None)    => ???
  case (None, Some(f))    => ???
  case (Some(a), Some(f)) => ???
}
```

Contrast this to the implementation of `bind` for `Option`: we can't know whether the `F[B]` produced by `A => F[B]` is `Some[B]` or `None` until we actually have a value of `A` to feed to the function `A => F[B]`.

This doesn't matter for `Option` so much (because it's just a simple type that either contains a value or does not), but consider what happens if we are building a description of a web service: if `A` is a value generated by a request (or produced by a response), we're not going to have access to that value until we actually serve up a request (or a response).

This is the reason why a monadic interface to building a web service description cannot be used to generate multiple representations: the "design" of the web service cannot be inspected until runtime, and at runtime, the "design" may change based on runtime values.

Applicatives have none of these issues.

## Applicative...Parser Combinators!

After I understood the core distinction between monads and mere applicative functors, I tried to come up with an applicative interface for building service descriptions.

My first attempt (a few lines of code I don't remember!) didn't go anywhere. But I didn't stop thinking about the problem.

I tried again in January of 2014.

This time, something occurred to me: describing web services is a *lot* like describing a grammar using parser combinators.

When you describe a parser using parser combinators, you put constraints on input and promise to produce values.

That's a bit like a web service, isn't it?

### Almost, Anyway

Parsers classically operate on linearly ordered input. While the HTTP protocol is indeed itself linear, describing it with classic parser combinators is cumbersome, because you have to order all your combinators in the order defined in the HTTP spec.

Moreover, with classic parser combinators, you have to ignore a lot of stuff that you consider irrelevant (browsers, for example, send all sorts of headers probably not required by your web service).

Encoding all the stuff your web service *doesn't need* would be very tedious and doesn't serve much point.

The solution is simplest enough. Don't use *classic* parser combinators!

In particular, I realized I could do the following:

 1. Break with the convention of a single input source so that parsers can "consume" input from the different types of data encoded in an HTTP request (headers, query strings, content, URLs, methods, etc.).
 2. For all types of data except content, allow the user to "consume" elements out of order (e.g. consuming a `Content-Type` header before an `Accept` header, assuming the request contains both).

I'm sure these things have a name, but for now I'm going with *multi-channel parser combinators*, defined as follows:

> **Multi-channel parser combinators are combinators that allow you to produce a single value by consuming input from a variety of channels, not necessarily in a linear fashion.**

RedEyes is built on this foundation.

## Multi-Channel Applicative Parser Combinators

At this point, you can probably imagine what our parser combinators are going to look like.

Here's a super greatly version that hints at the direction:

```scala
sealed trait Parser[A]
case class Header(name: String) extends Parser[String]
case class Query(name: String) extends Parser[String]
case class Content[A](decoder: Array[Byte] => A) extends Parser[A]
...
```

Our `Parser` is a generalized algebraic data type that *describes* a parser, but contains no functionality.

However, if you try to implement an `Applicative` interface for just the above type, you'll find you can't do it.

Why? Because there's no way to implement `map` or `ap`. However, we can easily solve that problem by adding to our GADT:

```scala
sealed trait Parser[A]
case class Header(name: String, value: String) extends Parser[String]
case class Query(name: String) extends Parser[String]
case class Content[A](decoder: Array[Byte] => A) extends Parser[A]
case class Pure[A](a: A) extends Parser[A]
case class Apply[A, B](fa: Parser[A], ff: Parser[A => B]) extends Parser[B]
```

The constructor `Apply` represent a *description* of application, but performs no computation.

The types of the parsers represent *promises* that such computations are possible, while a type-safe implementation represents a proof that such promises are actually possible to honor.

With this definition of `Parser`, writing an `Applicative` instance for `Parser` is trivial:

```scala
implicit val ParserApplicative = new Applicative[Parser] {
  def point[A](a: A): Parser[A] = Pure(a)

  def map[A, B](fa: => Parser[A])(f: A => B): Parser[B] = ap(fa)(point(f))

  def ap[A, B](fa: => Parser[A])(ff: Parser[A => B]): Parser[B] = Apply(fa, ff)
}
```

With this `Applicative` instance defined, we can build descriptions of web services using all the `Applicative` machinery that Scalaz brings to the table:

```scala
(Header("Content-Type", "application/json") |@| Query("limit"))((_, limit) => loadJsonData(limit))
```

(In Haskell, this would be much cleaner: `loadJsonData <$> header "Content-Type" "application/json" <*> query "limit"`.)

If we fleshed out our parser a bit more, I'm sure you can see how we could concisely describe a web service.

This formulation, which is very reminiscent of the `Free` monad (in that you're "recording" applications instead of actually performing them), turns out to be incredibly powerful.

### The Power of Applicatives

With this `Applicative` representation, we can traverse the entire GADT representing our service description without first having any values produced by an actual request.

So we can use the same description to build multiple things before we ever have to serve a request.

In particular, you can write a function to compile a service description down to a BlueEyes-style web service:

```scala
def compile[A](parser: Parser[A]): HttpRequest => Future[HttpResponse] = ???
```

(Although I wouldn't recommend it!)

You can also write a function to compile a service description down to documentation:

```scala
def document[A](parser: Parser[A]): Markdown = ???
```

In fact, for a more complex GADT than introduced above, we could also write an "optimizer" that takes one description, and yields another one which is equivalent but has a more efficient compilation:

```scala
def optimize[A](parser: Parser[A]): Parser[A] = ???
```

Even more incredibly, given a `Parser[A]`, we can create a generator for `A`'s that would be produced for some possible input:

```scala
def generate[A](parser: Parser[A]): EphemeralStream[A]
```

(`EphemeralStream` is a Scalaz abstraction that doesn't leak memory like Scala's own `Stream`, but it's the same idea.)

This would allow us to create "example inputs" which we could use for documentation purposes, instead of having to manually construct examples that may or may not be in sync with what the web service actually accepts. We could also use them for load testing or for fuzz testing.

Finally, the last feature I'll mention is that in the event of a malformed request, we know exactly what went wrong (e.g. the `Header("Content-Type", "application/json")` parser failed), and so we can generate helpful error messages or suggestions (e.g. "Expected Content-Type header to equal 'application/json'").

Wow! Starting to see the power of `Applicative`s yet?

## Trouble in Paradise

By the end of January 2014, I had sketched out a complete, if rudimentary interface to building web service descriptions using the "free Applicative" approach introduced here.

Unfortunately, I started noticing some serious problems in this formulation.

### Neglected Responses

The first problem I noticed is that this solution elegantly describes requests, but complete ignores responses.

While `Parser[A]` can be introspected, so you know the form of *input* required by the service, the `A` representing the value produced by the web service cannot be inspected until runtime (i.e. until a request is generated and a response is produced). Thus, you cannot statically analyze the form of response, and so you can't generate documentation or do anything else interesting.

This means that our representation for web services is inherently one-sided. The input to the service is described very well, but the output is not described at all.

I didn't consider this to be a *fatal* flaw in the approach. After all, usually everything but the structure of the response can be inferred from the request (for example, if a web service looks for the header `Accept: application/json`, it's a good bet the web service will respond with JSON content).

Nonetheless, this flaw manifests itself in far more than just documentation.

### Awful Client Libraries

To auto-generate a client library, you need to know not only the structure of the request, but the shape of the response, as well.

For example, if you don't know the content type of the response, then you'll have to leave it as text or binary. Exposing raw text or binary data to the user of a client library is not very useful (in fact, it rather defeats the point of a client library!).

### No Remoting

There's an unwritten law of web services: if you build a web service, you will ultimately end up consuming it &mdash; even if you wrote the web service for others to consume.

Ideally, you'd like to be able to consume web services that you create with zero boilerplate and in a type-safe way.

If I had my way, it would look something like this:

```scala
val serviceDesc = ???

val remoteService = remotely(serviceDesc)("http://myservice.com/api/v1/", 40)

val response = remoteService(request)
```

If something like this were possible, it would allow you to use the HTTP protocol for providing and consuming micro-services, in a completely type-safe and boilerplate-free way.

*If* it were possible.

