package scopt

import java.net.{ URL, UnknownHostException }
import collection.{ Seq => CSeq }

private[scopt] object platform {
  // java.lang.System.getProperty | SM | Documentation unclear, should be non-nullable.
  val _NL = System.getProperty("line.separator").nn

  import java.util.{ Locale, Calendar, GregorianCalendar }
  import java.text.SimpleDateFormat
  import java.io.File
  import java.net.{ InetAddress, URI }

  type ParseException = java.text.ParseException
  def mkParseEx(s: String | Null, p: Int) = new java.text.ParseException(s, p)

  trait PlatformReadInstances {
    // java.util.Locale.getDefault | SM | Documentation unclear, but should be non-nullable.
    def calendarRead(pattern: String): Read[Calendar] = calendarRead(pattern, Locale.getDefault.nn)
    def calendarRead(pattern: String, locale: Locale): Read[Calendar] =
      Read.reads { s =>
        val fmt = new SimpleDateFormat(pattern)
        val c = new GregorianCalendar
        c.setTime(fmt.parse(s))
        c
      }

    implicit val yyyymmdddRead: Read[Calendar] = calendarRead("yyyy-MM-dd")
    implicit val fileRead: Read[File] = Read.reads { new File(_) }
    // java.net.InetAddress.getByName | SM | Documentation unclear, but should be non-nullable.
    implicit val inetAddress: Read[InetAddress] = Read.reads { InetAddress.getByName(_).nn }
    implicit val uriRead: Read[URI] = Read.reads { new URI(_) }
    implicit val urlRead: Read[URL] = Read.reads { new URL(_) }
  }

  def applyArgumentExHandler[C](
      desc: String,
      arg: String): PartialFunction[Throwable, Either[CSeq[String], C]] = {
    case e: NumberFormatException =>
      Left(List(desc + " expects a number but was given '" + arg + "'"))
    case e: UnknownHostException =>
      Left(List(
        desc + " expects a host name or an IP address but was given '" + arg + "' which is invalid"))
    case e: ParseException if e.getMessage.contains("Unparseable date") =>
      Left(List(
        s"$desc date format ('$arg') couldn't be parsed using implicit instance of `Read[Date]`."))
    case _: ParseException =>
      Left(List(s"$desc expects a Scala duration but was given '$arg'"))
    case e: Throwable => Left(List(desc + " failed when given '" + arg + "'. " + e.getMessage))
  }

}
