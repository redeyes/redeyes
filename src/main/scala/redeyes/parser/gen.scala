package redeyes.parser

import scalaz._

import scalaz.syntax.monad._

trait ExampleGenerator { self: ParserModule =>
  protected def generateAtom[C <: Channel, A](channel: C, atom: Atom[C, A]): EphemeralStream[A]
  
  /**
   * Given a parser, returns a possibly infinite stream of values that
   * could be produced by the parser from some possible input.
   *
   * Note that the intersection of two infinite parsers is unlikely to 
   * produce anything in the lifetime of the universe. YMMV.
   */
  def generate[A](parser: Parser[A]): EphemeralStream[A] = {
    def generate0[A]: Parser[A] => Need[EphemeralStream[A]] = { (parser: Parser[A]) => 
      def generateOr[A](flattened: Vector[Need[Parser[A]]]) = {
        val streams = flattened.map(_.map(generate0).value)
    
        // Fair distribution (obviously may not match real distribution):
        EphemeralStream.unfold((0, streams)) {
          case ((cursorIndex0, streams)) =>
            var cursorIndex = cursorIndex0
            var searching = true
            var stream = streams(cursorIndex).value
            
            while (stream.isEmpty && searching) {
              cursorIndex = (cursorIndex + 1) % streams.length
              stream = streams(cursorIndex).value
              
              if (cursorIndex == cursorIndex0) searching = false
            }
            
            if (!searching) None
            else {
              val nextIndex = (cursorIndex + 1) % streams.length
        
              Some(stream.head() -> 
                (nextIndex -> streams.updated(cursorIndex, Need(stream.tail())))
              )
            }
        }
      }
      
      parser match {
        case End(_) => Need(EphemeralStream[A])
          
        case Fail => Need(EphemeralStream[A])

        case GetOrFail(parser) =>
          for {
            parser <- parser
            gen    <- generate0(parser)
          } yield gen.flatMap {
            case Some(x) => EphemeralStream(x)
            case None => EphemeralStream[A]
          }
        
        case Filter(parserN, f) => 
          for {
            parser <- parserN
            value  <- generate0(parser)
          } yield value.filter(f)
      
        case Pure(value) => 
          Need(EphemeralStream.iterate(value)(identity))
          
        case AtomParser(channel, atom) => 
          Need(generateAtom(channel.asInstanceOf[Channel], atom.asInstanceOf[Atom[Channel, A]])) // Scala type inference fail
          
        case LookAhead(parserN) => 
          parserN.flatMap(generate0)

        case x @ Intersect(left, right) =>
          for {
            left     <- left
            right    <- right
            leftGen  <- generate0(left)
            rightGen <- generate0(right)
          } yield leftGen.zip(rightGen).filter((x.equal.equal _).tupled).map(_._1)

        case Zip(left, right) => ???
          for {
            left     <- left
            right    <- right
            leftGen  <- generate0(left)
            rightGen <- generate0(right)
          } yield leftGen.zip(rightGen)

        case IfThenElse(predN, ifTrueN, ifFalseN) =>
          for {
            pred    <- predN
            predGen <- generate0(pred)

            ifTrue  <- ifTrueN

            ifFalse <- ifFalseN

            valGen  <-  Need(for {
                          bool <- predGen
                          valGen <- if (bool) generate0(ifTrue).value else generate0(ifFalse).value
                        } yield valGen)
          } yield valGen

        case x @ Eq(leftN, rightN) =>
          for {
            left    <- leftN
            leftGen <- generate0(left)

            right    <- rightN
            rightGen <- generate0(right)
          } yield leftGen.zip(rightGen).map(t => x.equal.equal(t._1, t._2))
      
        case x @ Join(leftN, rightN) => 
          for {
            left   <- leftN
            right  <- rightN
            leftG  <- generate0(left)
            rightG <- generate0(right)
          } yield {
            if (leftG.isEmpty) rightG
            else if (rightG.isEmpty) leftG
            else {
              leftG.zip(rightG).map(t => x.semigroup.append(t._1, t._2))
            }
          }
      
        case x @ Or(_, _) =>
          Need(generateOr(x.flatten))
      
        case Not(negated0, _) => for {
          negated <- negated0
          gen     <- generate0(negated)
        } yield gen

        case Repeat(parser0, min0, max0) =>
          for {
            parser <- parser0.asInstanceOf[Need[Parser[A]]] // Scala can't figure this out...
            stream <- generate0(parser)
          } yield {
            val min = min0.getOrElse(0)
            val max = max0.getOrElse(Int.MaxValue)
            
            val range = (max - min + 1).min(100)
        
            EphemeralStream.unfold((min, stream)) {
              case ((rep, stream)) =>          
                if (stream.isEmpty) None
                else {
                  val nextRep = ((rep + 1 - min) % range) + min
              
                  var remainder = stream
              
                  var i = 0
                  var v = Vector.newBuilder[A]
                  var matching = true
                  while (i < nextRep && matching) {
                    v += remainder.head()
                    remainder = remainder.tail()
                    if (remainder.isEmpty) matching = false // Unexpectedly ran out of input in a repetition!
                    i = i + 1
                  }
                  
                  if (i < nextRep) None
                  else Some(v.result -> (nextRep -> remainder))
                }
            }
          }
      
        case Map(valueN, f) => 
          for {
            value <- valueN
            gen   <- generate0(value)
          } yield gen.map(f)
      
        case x : Apply[_, _] =>  // Scala type inference fail
          for {
            f        <- x.f
            value    <- x.value
            fGen     <- generate0(f)
            valueGen <- generate0(value)
          } yield fGen.zip(valueGen).map(t => t._1(t._2))
      
        case Described(parserN, _, _) => parserN.flatMap(generate0)
      }
    }
    
    generate0(parser).value
  }
}