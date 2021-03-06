package lexi

/**
  * Created by gs on 19.09.17.
  */
sealed trait Regex {
  import Regex._

  def ~(other: Regex): Concat = Concat(this, other)
  def |(other: Regex): Alt    = Alt(this, other)
  def *(): Star               = Star(this)
  def +(): Regex              = this ~ this.*

  def alphabet: Set[Sym] = this match {
    case Epsilon        => Set.empty
    case Element(syms)  => syms
    case Concat(r1, r2) => r1.alphabet union r2.alphabet
    case Alt(r1, r2)    => r1.alphabet union r2.alphabet
    case Star(r)        => r.alphabet
  }
}

object Regex {
  object Epsilon extends Regex
  case class Element(syms: Set[Sym]) extends Regex
  case class Concat(r1: Regex, r2: Regex) extends Regex
  case class Alt(r1: Regex, r2: Regex) extends Regex
  case class Star(r: Regex) extends Regex

  def Element(c: Sym): Element = Element(Set(c))
  def strSeq(str: String): Regex =
    str.tail.foldLeft[Regex](Element(str.head)) { case (r, c) => Concat(r, Element(c)) }
  def strAny(str: String): Element =
    Element(str.toSet)

  implicit def stringToSeq(str: String): Regex = strSeq(str)

  implicit class RegexHelper(val sc: StringContext) extends AnyVal {
    def S(args: Any*) = { assert(sc.parts.size == 1); strSeq(sc.parts.head) }  // Character sequence from string
    def C(args: Any*) = { assert(sc.parts.size == 1); strAny(sc.parts.head) }  // Character class from string
  }

  val Alpha = C"abcdefghijklmnopqrstuvwxyz"
  val Digit = C"0123456789"
  val AlphaDigit = Alpha | Digit
}