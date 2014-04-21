# Introduction

This is an early, experimental preview release of RedEyes.

RedEyes is the spiritual successor to [BlueEyes](http://github.com/jdegoes/blueeyes/).

RedEyes is intended to be laser-focused on the following goal:

 * To provide a type-safe, composable, boilerplate- and macro-free way of creating external-facing HTTP services that must be well-documented and consumed by a variety of programming languages.

Eventually, RedEyes might be generalized to handle other text-oriented protocols.

# Status

RedEyes has an API for constructing services, but does not yet have a compiler to a specific server backend. The current plan is to produce a native backend for scalaz-stream over raw NIO. 

Many tests are missing and some of the interior surface area (especially the existing encoding of GADTs and the use of modules to reuse code across Char and Api parsers) could be greatly improved.

Pull requests welcome!

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

Let's say you write a handler to support HTTP GET with such and such a path, which requires some content type and produces another.

If you want to document this service, you'll have to replicate the exact same information in English! Why should you have to do that, when the information is already encoded into your program?

Indeed, documenting APIs, and keeping that documentation accurate and in sync with the code base, remains one of the biggest pains of developing and maintaining web APIs.

It was a big pain at SocialMedia.com, the company where I initially developed BlueEyes, and it was an even bigger pain at Precog, the next company I started. Countless weeks were spent writing and re-writing documentation when we changed the API, and yet more time was spent helping customers track down problems caused by missing or inaccurate documentation.

### Client Libraries

Sometimes, all you need to expose your services is the web API. But other times, the barrier of entry is still too high, and to lower that barrier, you end up needing to create client libraries.

Client libraries wrap the web API and provide language-specific, idiomatic interfaces to the same functionality.

Like documentation, they are extremely painful to develop and keep up to date. You often need to create 4 - 6 different client libraries, one for every mainstream programming language (plus 1 for the niche functional language that you use!).

It will take at least 2 weeks to develop a minimal client library for a relatively straightforward API, so we are talking months at the minimum to support mainstream languages.

As with documentation, your code already describes what your web API expects.

Wouldn't it be nice if you could somehow use this information to avoid manually developing client libraries?

## Fling with Metadata

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

Clearly, some better abstraction was needed to allow developers to describe their services, and then based on that description, generate request handlers, documentation, and so forth. A sort of *cross-compilation* of *service descriptors* to multiple targets, if you will.

But what that abstraction looked like remained a complete mystery for a long while.

## The Long Winter

For two long years, BlueEyes remained essentially unchanged. Various improvements were made to content handling, bugs were fixed, and combinators added &mdash; but the core API didn't change much.

Meanwhile, I wasn't coding anymore, and my rate of learning slowed to a crawl. I spent most of my time in meetings, email, and docs.

But I did think about BlueEyes, and about possible solutions to the problem of duplicated information.

In June 2013, my company Precog was acquired by RichRelevance. I stuck around for a few months, but decided I didn't want to work there, so I left and joined the ranks of independent consultants. A hired gun for anything from architecture to implementation.

After 2 years of stagnation, I finally had a chance to code again. To read papers. To dig into good libraries. To level-up as a functional programmer.

It was in December 2013 that I read in a blog post somewhere that applicative functors were suited for static analysis, while monads were not.

I thought static analysis of "service descriptors" might be a way to generate multiple things from the same description: request handlers, documentation, maybe even client libraries.

*That* caught my attention.

## Applicative Functors to the Rescue

Every monad forms an applicative functor, but most libraries that implement an applicative interface also implement a monadic one.

As a result, I always used monadic interfaces, and never closely examined the differences between the two.

But reading that blog post made me stop and think. I immediately dug into the `Applicative` functor type class:

```scala
def map[A, B](fa: F[A])(f: A => B): F[B]

def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B]

def point[A](a: => A): F[A]

```

Compare this with the `Monad` type class, which inherits all of the above but adds the following functionality:

```scala
def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
```

Compare the function `ap` to the function `bind`. They are obviously similar, and in fact they're often used for the same purpose.

Both `bind` and `ap` accept a functor, as well as a function that uses a value inside the functor to produce a value of a different type.

In other words, both `bind` and `ap` are "transformation" functions. They transform the value inside the functor to a value of another type. 

The differences in the types tells you everything you need to know:

> **When you call `bind` you can, at runtime, decide which `F[B]` you want to produce based on the value of `A`. However, when you call `ap`, you have already decided on both `F[A]` and `F[A => B]`. Your only runtime decision is which `B` you want to produce given an `A`.**

Monads allow context-sensitive transformations, while merely applicative functors deny them.

This is what makes monads so ungodly powerful. But with great power comes, well: a black box that you can't peer into.

With `Applicative` functors, you can peek in the box.

### Peering into Applicative Functors

One of the simplest possible `Applicative` functors is `Option`.

If we're writing the implementation of `ap`, we know whether `F[A]` is `Some[A]` or `None`, and we know whether `F[A => B]` is `Some[A => B]` or `None`. In fact, here's some code to discriminate on all the possibilities:

```scala
def ap[A](fa: Option[A])(f: Option[A => B]): Option[B] = (fa, f) match {
  case (Some(a), None)    => ???
  case (None, Some(f))    => ???
  case (Some(a), Some(f)) => ???
}
```

Contrast this to the implementation of `bind` for `Option`: we can't know whether the `F[B]` produced by `A => F[B]` is `Some[B]` or `None` until we have a value of `A` to feed to the function `A => F[B]`.

This doesn't matter for `Option` so much, but consider what happens if we are building a description of a web service: if `A` is a value generated by a request (or produced by a response), we're not going to have access to that until runtime.

This is the reason why a monadic interface to building a web service description will fail, while an applicative interface can succeed.

## Applicative...Parser Combinators!

After I understood the core distinction between monads and mere applicative functors, I tried to come up with an applicative interface for building service descriptions.

My first attempt (a few lines of code I don't have anymore!) didn't go anywhere. But I didn't stop thinking about the problem.

I tried again in January of 2014. This time, something popped out: describing web services is a *lot* like describing a grammar using parser combinators.

When you describe a parser using parser combinators, you put constraints on input and promise values in return.

That's a bit like a web service, isn't it?

Yes, indeed, with a little difference that bears discussing.

### Almost, Anyway

Parsers classically operate on linearly ordered input. While the HTTP protocol is indeed itself linear, describing it with classic parser combinators is extremely cumbersome, because you have to order all your combinators in the order defined in the HTTP spec.

Moreover, with classic parsers, you have to ignore a lot of stuff that you consider irrelevant (browsers, for example, send all sorts of headers probably not required by your web service). Encoding all the stuff your web service *doesn't* need would be very tedious and doesn't serve much point.

So we need to do two things:

 1. Break with the convention of a single input source so that parsers can "consume" input from the different types of data encoded in an HTTP request (headers, query strings, content, URLs, methods, etc.).
 2. Depending on the type of data, break with the convention of linearity of input. For example, you shouldn't have to choose an order if your web service requires both the header `Content-Type` and the header `Accept`. (Technically, this can and probably should be solved with a combinator that has order-independent parsing -- which is possible to do in a type-safe way using dependent types.)

I'm sure these things have a name, but for now I'm going with *multi-channel parser combinators*, defined as follows:

> **Multi-channel parser combinators are combinators that allow you to produce a single value by consuming from a variety of channels, not necessarily in a linear fashion.**

RedEyes is built on this foundation.

## Multi-Channel Applicative Parser Combinators

At this point, you can probably imagine what our parser combinators are going to look like.

Here's a super simplified version that hints at the direction:

```scala
sealed trait Parser[A]
case class Header(name: String) extends Parser[String]
case class Query(name: String) extends Parser[String]
case class Content[A](decoder: Array[Byte] => A) extends Parser[A]
...
```

So our `Parser` is a generalized algebraic data type that *describes* our parsers, but contains no functionality (it's like a functor for a free monad, if you're familiar with that method of encoding effects as data with an interpreter).

However, if you try to implement an `Applicative` interface for just the above type, you'll find you can't do it.

Why? Because there's no way to implement `map` or `ap`. However, we can easily solve that problem by adding to our GADT:

```scala
sealed trait Parser[A]
case class Header(name: String) extends Parser[String]
case class Query(name: String) extends Parser[String]
case class Content[A](decoder: Array[Byte] => A) extends Parser[A]
case class Pure[A](a: A) extends Parser[A]
case class Apply[A, B](fa: Parser[A], ff: Parser[A => B]) extends Parser[B]
```

The constructor `Apply` represent a *description* of mapping and application, but they perform no computation. Their types represent *promises* that such a computation is possible (while a type-safe implementation would represent a proof that such promises are actually possible to keep).

Now writing an `Applicative` instance for `Parser` is trivial:

```scala
implicit val ParserApplicative = new Applicative[Parser] {
  def point[A](a: A): Parser[A] = Pure(a)

  def map[A, B](fa: => Parser[A])(f: A => B): Parser[B] = ap(fa)(point(f))

  def ap[A, B](fa: => Parser[A])(ff: Parser[A => B]): Parser[B] = Apply(fa, ff)
}
```

This formulation turns out to be amazingly powerful.

### The Power of Applicatives

With this `Applicative` representation, we can traverse the entire GADT representing our service description without first having any concrete values produced by an actual request.

So we can use the same description to build multiple things in advance of serving requests.

In particular, you can write a function to compile a service description down to a BlueEyes-style request handler:

```scala
def compile[A](parser: Parser[A]): HttpRequest => Future[HttpResponse] = ???
```

(Although I wouldn't recommend it!)

You can also write a function to compile a service description down to documentation:

```scala
def document[A](parser: Parser[A]): String = ???
```

In fact, for a more complex GADT, we could also write an "optimizer" that takes one description, and yields another one which is equivalent but has a more efficient compilation:

```scala
def optimize[A](parser: Parser[A]): Parser[A] = ???
```

Even more incredibly, given a `Parser[A]`, we can create a generator for `A`'s that might be produced on some successful parse:

```scala
def generate[A](parser: Parser[A]): EphemeralStream[A]
```

(`EphemeralStream` is a Scalaz abstraction that doesn't leak memory like Scala's own Stream, but it's the same idea.)

This would allow us to create "example inputs" which we could use for documentation purposes (instead of having to manually construct examples that may or may not be in sync with what the web service actually accepts).

Finally, the last magical feature of this representation I'll talk about is that in the event of a malformed request, we know exactly what went wrong (e.g. the `Header("Content-Type")` parser failed), and so we can generate helpful error messages or suggestions (e.g. "Expected Content-Type header to match 'application/json'").

Wow! Starting to see the power of `Applicative`s yet?

### Remaining Goals

I've (roughly) shown how `Applicative`s allow us to describe our web service one time, and then use the same description to generate everything from an executable service to documentation.



