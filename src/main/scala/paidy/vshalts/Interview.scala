package paidy.vshalts

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import java.time.format.DateTimeFormatter

object Interview {

  private val dateFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")

  // Use regexp from Play as it compatible with the W3C recommendation
  // https://github.com/playframework/playframework/blob/2.8.x/core/play/src/main/scala/play/api/data/validation/Validation.scala#L79
  // https://html.spec.whatwg.org/multipage/input.html#valid-e-mail-address
  private val emailRegex =
    """^([a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+)@([a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*)$""".r
  private val phoneRegex = """^\+?(\d\s?){9,}$""".r

  /**
    * Implemented according too
    * https://en.wikipedia.org/wiki/Ordinal_indicator
    */
  def toOrdinalString(number: Long): String = {
    // We can do better in Scala than throws exceptions, for simplicity will use exceptions.
    if (number < 0)
      throw new IllegalArgumentException("Unsupported number")

    val suffix = number % 100
    val teenNumbers = suffix > 10 && suffix < 20
    if (teenNumbers)
      s"${number}th"
    else
      number % 10 match {
        case 1 => s"${number}st"
        case 2 => s"${number}nd"
        case 3 => s"${number}rd"
        case _ => s"${number}th"
      }
  }

  /**
    * Counts the number of Sundays between dates. Expecting correct range: firstDate <= secondDate
    */
  def sundaysBetween(firstDateStr: String, secondDateStr: String): Long = {
    val firstDate = LocalDate.parse(firstDateStr, dateFormatter)
    val secondDate = LocalDate.parse(secondDateStr, dateFormatter)

    if (firstDate.compareTo(secondDate) > 0)
      throw new IllegalArgumentException("Incorrect date range")

    val daysBetween = ChronoUnit.DAYS.between(firstDate, secondDate)
    val firstDayOfWeek = firstDate.getDayOfWeek().getValue
    val daysToNextSunday = 7 - firstDayOfWeek

    if (daysBetween >= daysToNextSunday) {
      val daysLeft = daysBetween - daysToNextSunday
      val wholeWeeksLeft = daysLeft / 7
      wholeWeeksLeft + 1
    } else
      0
  }

  /**
   * Obfuscate email and phones
   */
  def obfuscate(str: String) = {

    def obfuscateEmail(account: String, domain: String) = {
      val obfuscatedAccount =
        if (account.length >= 2) {
          s"${account.head}*****${account.last}"
        } else {
          // Requirements "all characters in the local-part between the first and last" unclear
          // What we should do if we have only 1 character?
          // Doing the best guess...
          s"${account.head}*****"
        }

      s"$obfuscatedAccount@$domain"
    }

    def obfuscatePhone(phone: String): String = {
      val totalDigits = phone.count(_.isDigit)
      val res = new StringBuilder
      phone.foldLeft(totalDigits - 4) {
        case (toObfuscate, c) =>
          if (c.isDigit) {
            if (toObfuscate == 0) {
              res.append(c)
              0
            } else {
              res.append("*")
              toObfuscate - 1
            }
          } else {
            res.append(if (c == ' ') "-" else c)
            toObfuscate
          }
      }
      res.toString
    }

    str match {
      case null => throw new NullPointerException("Incorrect input")
      case e if e.trim.isEmpty =>
        throw new NullPointerException("Incorrect input")
      case e if phoneRegex.findFirstMatchIn(e).isDefined => obfuscatePhone(str)
      case emailRegex(emailAccount, emailDomain) =>
        obfuscateEmail(emailAccount, emailDomain)
      case _ =>
        throw new IllegalArgumentException("String format not recognized")
    }
  }
}
