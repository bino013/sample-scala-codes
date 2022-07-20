import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, LocalDate}

import scala.util.matching.Regex

object PaidyTest {

  def main(args: Array[String]): Unit = {
    println(toOrdinal(0))
    println(toOrdinal(10))
    println(toOrdinal(20))

    println(toOrdinal(1))
    println(toOrdinal(11))
    println(toOrdinal(21))

    println(toOrdinal(2))
    println(toOrdinal(12))
    println(toOrdinal(22))

    println(toOrdinal(3))
    println(toOrdinal(13))
    println(toOrdinal(23))

    println(countSundays("01-01-2021", "31-12-2021"))

    println(obfuscateEmailAndPhoneNumber("a@google.com"))
    println(obfuscateEmailAndPhoneNumber("arvin.caleon@google.com"))
    println(obfuscateEmailAndPhoneNumber("a@google.com"))
    println(obfuscateEmailAndPhoneNumber("a@google.ph"))
    println(obfuscateEmailAndPhoneNumber("juan.dela.cruz@up.edu.ph"))
    println(obfuscateEmailAndPhoneNumber("juan.dela.cruz@uste.up.edu.ph"))
    println(obfuscateEmailAndPhoneNumber("Juan.d3|4.C+uZ@uste.up.edu.ph"))
    //println(obfuscateEmailAndPhoneNumber("ju@n.d3l4.C+uZ@uste.up.edu.ph")) error

    println(obfuscateEmailAndPhoneNumber("+4 1 2 3      4 5 6 7 8 9 0   9"))
    println(obfuscateEmailAndPhoneNumber("+44 1234 456 789"))
    //println(obfuscateEmailAndPhoneNumber("+44 1234 456+ 789+")) not valid

  }

  def toOrdinal(value: Int): String = {
    def appendTH(str: String): String = s"${str}th"

    if (value == 11 || value == 12 || value == 13)
      appendTH(value.toString)
    else
      value.toString match {
        case str if str.endsWith("1") => s"${str}st"
        case str if str.endsWith("2") => s"${str}nd"
        case str if str.endsWith("3") => s"${str}rd"
        case str                      => appendTH(str)
      }
  }

  private val DateFormat = DateTimeFormatter.ofPattern("dd-MM-yyyy")

  def countSundays(start: String, end: String): Int = {
    val from = LocalDate.parse(start, DateFormat)
    val to = LocalDate.parse(end, DateFormat)

    if (to.isBefore(from))
      throw new IllegalArgumentException("end date cannot be before start date")
    else
      from.toEpochDay
        .to(to.toEpochDay)
        .map(LocalDate.ofEpochDay)
        .count(_.getDayOfWeek == DayOfWeek.SUNDAY)

  }

  private val EmailRegex: Regex =
    """^[a-zA-Z0-9.!#$%&'*+/?^_`|~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

  private val PhoneNumberRegex =
    """^(\+\d{1,9})\s*\d{1,9}\s*\d{1,9}\s*\d{1,9}\s*\d{1,9}\s*\d{1,9}\s*\d{1,9}\s*\d{1,9}\s*\d{1,9}([\s\d]+)$""".r

  def obfuscateEmailAndPhoneNumber(value: String): String = {
    if (EmailRegex.matches(value)) {
      val emailSplit = value.toLowerCase.split("@")
      val localPart = emailSplit.head
      s"${localPart.head}*****${localPart.last}@${emailSplit.last}"
    } else if (PhoneNumberRegex.matches(value)) {
      def showLastFourDigit(obfuscated: String): String = {
        var lastDigitCount = 0
        var index = obfuscated.length - 1
        var newObfuscated = obfuscated
        while (lastDigitCount != 4) {
          if (obfuscated(index) == '*') {
            newObfuscated = newObfuscated.updated(index, value(index))
            lastDigitCount = lastDigitCount + 1
          }
          index = index - 1
        }
        newObfuscated
      }

      val obfuscated = value.replaceAll("\\s", "-").replaceAll("\\d", "*")
      showLastFourDigit(obfuscated)
    } else {
      throw new IllegalArgumentException(
        s"$value is not a valid email or phone number"
      )
    }
  }

}
