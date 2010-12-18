package nl.flotsam.scalendar

import scala.util.parsing.combinator._

/** A parser of iCalendar files, based on RFC5545. (However, this one
  * doesn't make a distinction between folded and non-folded text, and
  * probably is way too tolerant and a little too restrictive in some
  * cases.)
  * 
  * @see [[http://tools.ietf.org/html/rfc5545]] */
object CalendarParser extends RegexParsers {

  def contentlines: Parser[Any] = rep(contentline)

  def contentline: Parser[Any] = name ~ rep(";" ~ param) ~ ":" ~ value

  def name: Parser[Any] = ianatoken | xname

  def ianatoken: Parser[Any] = """[a-zA-Z0-9\-]+""".r

  def xname: Parser[Any] = "X-" ~ opt(vendorid ~ "-") ~ """[a-zA-Z0-9\-]""".r

  def vendorid: Parser[Any] = """[a-zA-Z0-9]3""".r

  def param: Parser[Any] = paramName ~ "=" ~ paramValue ~ rep("," ~ paramValue)

  def paramName: Parser[Any] = ianatoken | xname

  def paramValue: Parser[Any] = paramText | quotedString

  def paramText: Parser[Any] = """[^\";.:]*""".r

  def quotedString: Parser[Any] = """\"[^\"]*\"""".r

  def value: Parser[Any] = """[^\r]*\r\n([\t\ ]+[^\r]*\r\n)*""".r

}

object ParseICalendar {

  import java.io._
  import CalendarParser._

  def main(args: Array[String]) {
    val reader = new FileReader(args(0))
    println(parseAll(contentlines, reader))
  }

}
