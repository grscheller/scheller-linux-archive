/** Parsing package */
package fpinscala.parsing

import scala.language.higherKinds
import scala.language.implicitConversions

// Parser is a type parameter that is itself a covariant type constructor.
// A new concept.

trait Parsers[ParseError, Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  // Some Laws:
  //   run(char(c))(c.toString) == Right(c)
  //   run(string(s))(s) == Right(s)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p1: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p1, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p1, p2)
  }

}
