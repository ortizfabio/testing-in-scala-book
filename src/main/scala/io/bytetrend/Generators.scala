package io.bytetrend

import java.time.LocalDateTime

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.scalacheck.{Arbitrary, Gen}

import scala.io.Source
import scala.util.Random
import Constants._

trait Generators {

  val random = new scala.util.Random

  val XML_HEADER = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>"""

  /**
    * Generate a random string of length n from the given alphabet
    * @param alphabet
    * @param n
    * @return
    */
  def randomString(alphabet: String)(n: Int): String =
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString

  /**
    * Generate a random alphabnumeric string of length n
    * @param n
    * @return
    */
  def randomAlphanumericString(n: Int) = randomString("abcdefghijklmnopqrstuvwxyz0123456789")(n)

  /**
    * When generating random strings of arbitrary length this function is called so that
    * the length of the string assigned to the variable meets the requirements for such
    * variable.
    * @param s generated random string.
    * @param maxLength maximum length allowed.
    * @return string upto with length upto maxLength.
    */
  def maxLength(s: String, maxLength: Int): String = {
    (s.length - maxLength) match {
      case 0 => s
      case n if n < 0 => s.concat(randomAlphanumericString(-1 * n))
      case n if n > 0 => s.substring(0, maxLength)
    }
  }

  /**
    * Timecode reference to a moment in time that recently passed.
    */
  val now = new DateTime()

  /**
    * A time index that is helpful in identifying large data sets. Appears as a string such as 139.240, representing the
    * day of the year followed by the minute of the day. This can be used as a tag, for example, at the beginning of a test
    * string, thereby helping to identify the data set based on when it was generated.
    */
  val timeIndex = s"${now.dayOfYear().get()}.${now.minuteOfDay().get()}"

  /**
    * A set of characters usable in generating random phone numbers.
    */
  val phoneNumberSet: Seq[Char] = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ' ', ',', '-')

  /**
    * A set of characters usable in phone numbers punctuation or separators.
    */
  val phoneNumberSeparators: Seq[Char] = List(' ', ',', '-', '+', '#')

  /**
    * A ScalaCheck generator that creates random phone number digit sequences.
    */
  val phoneNumberCharacter: Gen[Char] = Gen.oneOf(phoneNumberSet)

  /**
    * A very limited set of Unicode characters, good when generating "pronouncible looking" strings. Not a particularly good
    * data set for testing though (consider using the unicodeCharacter type instead).
    */
  val smallUnicodeSet = ('A' to 'Z') ++ ('a' to 'z') ++ ('À' to 'ÿ')

  /**
    * A ScalaCheck generator that creates pronouncible-looking character sequences.
    */
  val pronouncibleLatinCharacter: Gen[Char] = Gen.oneOf(smallUnicodeSet)

  /**
    * A ScalaCheck generator that creates character sequences from a wide range of Latin characters.
    */
  val latinUnicodeCharacter = Gen.choose('\u0041', '\u01B5').filter(Character.isDefined)

  /**
    * A ScalaCheck generator that creates character sequences from the entire range of Unicode characters. This includes
    * Asian languages and symbols.
    */
  val unicodeCharacter = Gen.choose(Char.MinValue, Char.MaxValue).filter(Character.isDefined)

  // MARK: Reusable generators that will output parameterized test values:

  /**
    * A Unicode string generator useful in testing. Can be used to generate random sequences of characters of an arbitrary
    * length.
    *
    * @param generator The character generator to use, an instance of Gen[Char]. Defaults to unicodeCharacter.
    * @param minimum   An Int that specifies the shortest string to generate.
    * @param maximum   An Int that specifies the longest string to generate.
    * @return A randomly generated string.
    */
  def unicodeGenerator(generator: Gen[Char] = Gen.alphaChar, minimum: Int = 5, maximum: Int = 20): Gen[String] = Gen.chooseNum(minimum, maximum).flatMap { n =>
    Gen.buildableOfN[String, Char](n, generator)
  }

  /**
    * A Unicode string generator useful in testing. Can be used to generate random sequences of characters of an arbitrary
    * length. The string will have any white space or horizontal space, including horizontal control characters, stripped from
    * it. This can be helpful when generating data sets for fields that disallow white space.
    *
    * Note that this implementation strips horizontal white space from the string after the string is generated. For this reason
    * the minimum length is a guideline, and the actual returned string may be significantly shorter than the specified minimum
    * length.
    *
    * @param generator The character generator to use, an instance of Gen[Char]. Defaults to unicodeCharacter.
    * @param minimum   An Int that specifies the shortest string to generate.
    * @param maximum   An Int that specifies the longest string to generate.
    * @return A randomly generated string.
    */
  def unicodeGeneratorNoWhitespace(generator: Gen[Char] = Gen.alphaChar, minimum: Int = 5, maximum: Int = 20): Gen[String] = Gen.chooseNum(minimum, maximum).flatMap { n =>
    Gen.buildableOfN[String, Char](n, generator).map(_.replaceAll("[\\p{Z}\\p{C}]", ""))
  }

  /**
    * Generates a random list of words (using the words.txt file as a source for words, selecting them at random).
    *
    * @param wordGenerator A `Gen[String]` that returns a word (either `wordGenerator` or `latinGenerator`, depending on
    *                      whether you want English words or Latin words, respectively.
    * @param minimum The fewest number of words to return.
    * @param maximum The most number of words to return.
    * @return A `Gen[List[String]]` containing between `minimum` and `maximum` words.
    */
  def sentenceGenerator(wordGenerator: Gen[String] = latinGenerator, minimum: Int = 1, maximum: Int = 20): Gen[List[String]] = Gen.chooseNum(minimum, maximum).flatMap { n =>
    Gen.containerOfN[List, String](n, wordGenerator)
  } map { n =>
    (n.head.capitalize :: n.tail.dropRight(1)) :+ n.last + "."
  }

  // TODO ZJB clean up
  lazy val regionsSourceUS: List[String] = Source.fromURL(getClass.getResource("/us_postal_codes.csv")).getLines().toList.drop(1) // drop the header
  lazy val zipcodeSource: List[String] = regionsSourceUS.map(_.split(',').head).distinct
  lazy val citySource: List[String] = regionsSourceUS.map(_.split(',').drop(1).head).distinct

  lazy val wordSource = Source.fromURL(getClass.getResource("/words.txt")).getLines().toList
  lazy val latinSource = Source.fromURL(getClass.getResource("/latin.txt")).getLines().toList
  lazy val portsOfCallsSource = Source.fromURL(getClass.getResource("/from_to_ports.txt")).getLines().toList
  lazy val countriesSource = Source.fromURL(getClass.getResource("/country_list.txt")).getLines().toList
  //  lazy val citySource = Source.fromURL(getClass.getResource("/city_list.txt")).getLines().toList
  lazy val stateSource = Source.fromURL(getClass.getResource("/state_list.txt")).getLines().toList
  //  lazy val zipcodeSource = Source.fromURL(getClass.getResource("/zipcode_list.txt")).getLines().toList
  lazy val addressSource = Source.fromURL(getClass.getResource("/address_list.txt")).getLines().toList

  /**
    * A word generator that returns a single, random word taken from the "resources/words.txt" file.
    *
    * @return A `String` representing a single word.
    */
  def wordGenerator = Gen.chooseNum(0, wordSource.length - 1).map(wordSource(_))
  def latinGenerator = Gen.chooseNum(0, latinSource.length - 1).map(latinSource(_))
  def portsOfCallGenerator: Gen[String] = Gen.choose(0, portsOfCallsSource.length - 1).map(portsOfCallsSource(_))
  def countryGenerator: Gen[String] = Gen.choose(0, countriesSource.length - 1).map(countriesSource(_))
  def cityGenerator: Gen[String] = Gen.chooseNum(0, citySource.length - 1).map(citySource(_))
  def stateGenerator: Gen[String] = Gen.chooseNum(0, stateSource.length - 1).map(stateSource(_))
  def zipcodeGenerator: Gen[String] = Gen.chooseNum(0, zipcodeSource.length - 1).map(zipcodeSource(_))
  def addressGenerator: Gen[String] = Gen.chooseNum(0, addressSource.length - 1).map(addressSource(_))

  /**
    * Generates a random list of names (using the names.txt file as a source for names, selecting them at random).
    *
    * @param nameGenerator Either `europeanNameGenerator` or `internationalNameGenerator`. The default is `internationalNameGenerator`.
    * @param minimum The fewest number of names to return.
    * @param maximum The most number of names to return.
    * @return A `Gen[List[String]]` containing between `minimum` and `maximum` names.
    */
  def fullNameGenerator(nameGenerator: Gen[String] = internationalNameGenerator, minimum: Int = 1, maximum: Int = 5): Gen[List[String]] = Gen.chooseNum(minimum, maximum).flatMap { n =>
    Gen.containerOfN[List, String](n, nameGenerator)
  }

  lazy val nameSource = Source.fromURL(getClass.getResource("/names.txt")).getLines().toList

  /**
    * A name generator that returns a single, random name taken from the "resources/names.txt" file.
    *
    * @return A `String` representing a single name.
    */
  def europeanNameGenerator = Gen.chooseNum(0, nameSource.length - 1).map(nameSource(_))

  /**
    * A name generator that creates names by combining European names (above) with randomly generated names based on Latin and multinational characters.
    *
    * @return A `String` representing a single name.
    */
  def internationalNameGenerator = Gen.oneOf(Gen.chooseNum(0, nameSource.length - 1).map(nameSource(_)), randomStringGenerator(unicodeGenerator(unicodeCharacter)), randomStringGenerator(unicodeGenerator(pronouncibleLatinCharacter)))

  /**
    * Generates a short code consisting of alpha-numeric characters. The returned String will be between 3 and 10 characters
    * long (inclusive).
    */
  val shortCodeGenerator: Gen[String] = Gen.chooseNum(3, 10).flatMap { n =>
    Gen.sequence[String, Char](List.fill(n)(Gen.alphaNumChar))
  }
  /**
    * A port of call that returns a single, random ISO port code taken from the "resources/port_of_call.txt" file.
    *
    * @return A `String` representing a ISO code for a port.
    */
  def portOfCallGenerator = Gen.chooseNum(0, portsOfCallsSource.length - 1).map(portsOfCallsSource(_))

  /**
    * Generates random phone numbers of varying length.
    *
    * @return A tuple containing two strings, a prefix and a phone number. Use `mkString` to create a single phone number value.
    */
  val phoneNumberGenerator: Gen[List[String]] = Gen.chooseNum(6, 18).flatMap { n =>
    for {
      prefix <- Gen.oneOf("1", "011", "0", "91")
      phone <- Gen.sequence[String, Char](List.fill(n)(phoneNumberCharacter))
    } yield List(prefix, phone.trim.stripSuffix(",").stripSuffix("-"))
  }

  /**
    * Generators one phone number separator symbol.
    */
  val phoneNumberSeparatorGenerator: Gen[Char] = Gen.oneOf(phoneNumberSeparators)

  /**
    * Generates random email addresses using the specified generator. If not specified, the unicodeGeneratorNoWhitespace will
    * be used. Optionally, a time index can be prepended to the email address to aid in identification of test values.
    *
    * @param generator A generator to use. Defaults to `fullNameGenerator` but other options would be `unicodeGeneratorNoWhitespace`.
    * @param withIndex Whether or not to include a time index at the beginning of the email address. Defaults to false.
    * @return A String containing a randomly generated email address.
    */
  def emailGenerator(generator: Gen[String] = fullNameGenerator() map (_.mkString(Gen.oneOf(".", "-").sample.getOrElse(""))), withIndex: Boolean = false): Gen[String] = generator map { s =>
    ((if (withIndex) timeIndex else "") + s + "@" + Gen.oneOf(simpleRandomShortCode() + ".", "").sample.getOrElse("") + wordGenerator.sample.getOrElse("google") + ".com")
  }

  /**
    * Generates random Strings that could be used for anything. If not specified, the unicodeGenerator will be used. Optionally,
    * a time index can be prepended to the name to aid in identification of test values. If more pronouncible names are
    * desired, unicodeGenerator(pronouncibleLatinCharacter) is a good option. although this dramatically reduces the quality
    * of compliance testing.
    *
    * @param generator A generator to use. Defaults to unicodeGenerator.
    * @param withIndex Whether or not to include a time index at the beginning of the name. Defaults to false.
    * @return A String containing a randomly generated name.
    */
  def randomStringGenerator(generator: Gen[String] = unicodeGenerator(), withIndex: Boolean = false): Gen[String] = generator.map(s => ((if (withIndex) timeIndex else "") + s.trim.capitalize))

  // MARK: Basic random value patterns:

  /**
    * Given an email address in a `String`, randomly mutates the case of the email address, returning a `List` with the
    * mutations. At least one all-uppercase and one all-lowercase mutation will be included.
    *
    * @param email A `String` representing an email address to use as a base pattern.
    * @param size  The number of mutations to return in the `List`.
    * @return A `List[String]` containing the specified number of email address (at least two will always be returned).
    */
  def randomFormattedEmailList(email: String, size: Int): List[String] = {
    email.toUpperCase +: email.toLowerCase +: List.fill(size - 2) {
      email.toVector.map(i => if (Random.nextInt(2) % 2 == 0) i.toUpper else i.toLower).mkString
    }
  }

  /**
    * Generates a simple, randomized short code of up to seven digits (unless overridden with a different size) consisting
    * of alphanumeric characters.
    *
    * @return A String containing the shortcode.
    */
  def simpleRandomShortCode(length: Int = 7): String = {
    Random.alphanumeric.take(Random.nextInt(length) + 1).mkString
  }

  /**
    * Generates a simple random email with a minimum length of length:. The email will be significantly longer than the
    * specified length.
    *
    * @param length The base length of the first part of the email. The overall email will be longer.
    * @return A String containing a randomly generated email.
    */
  def simpleRandomEmail(length: Int = 18): String = {
    //Total max length of 50
    maxLength(Random.alphanumeric.take(length).mkString, 20) + "@" + maxLength(simpleRandomShortCode(), 6) + ".accenture.com"
  }

  /**
    * Generates a simple name using basic alphabetic (Latin) characters. The name length will be up to the provided length
    * (and will be at least two characters). It will be in proper case.
    *
    * @param length The maximum length of the generated name.
    * @return A String containing a randomized name-like string.
    */
  def simpleRandomName(length: Int = 12): String = {
    Random.alphanumeric.filter(_.isLetter).take(Random.nextInt(length) + 2).mkString.toLowerCase.capitalize
  }

  /**
    * Generates a simple random password using alphanumeric characters, and appends a single symbol to the end of the
    * password. These will be adequate to pass the password requirements.
    *
    * @param length Minimum length of the password.
    * @return A String consisting of alphanumeric characters plus at least one symbol.
    */
  def simpleRandomPassword(length: Int = 20): String = {
    Random.alphanumeric.take(length).mkString + "a0#"
  }

  /**
    * Generates a simple alphanumeric string.
    *
    * @return Returns an arbitrary length String containing alphanumeric characters (usually of at least 6 characters in
    *         length).
    */
  def simpleRandomNumericalString(): String = {
    (100000.0 + Random.nextDouble * (10000000000.0 - 100000.0)).toLong.toString
  }

  /**
    * Generates a simple random phone number, consisting of two numbers separated by a hyphen, limited to 20 characters
    * maximum length.
    *
    * @return A simple, randomly generated phone number `String` of up to 20 characters in length.
    */
  def simpleRandomPhoneNumber(): String = {
    (1234567890.0 + Random.nextDouble * (1.0 - 100000.0)).toLong.toString
  }

  val fmtYyyyMMdd: DateTimeFormatter = DateTimeFormat.forPattern("yyyyMMdd")
  val fmtMMddyyyy: DateTimeFormatter = DateTimeFormat.forPattern("MMddyyyy")
  val fmtHhmmss: DateTimeFormatter = DateTimeFormat.forPattern("hhmmss")
  val fmtYy: DateTimeFormatter = DateTimeFormat.forPattern("yy")
  val fmtMM: DateTimeFormatter = DateTimeFormat.forPattern("MM")
  val fmtDd: DateTimeFormatter = DateTimeFormat.forPattern("dd")
  val fmtHh: DateTimeFormatter = DateTimeFormat.forPattern("hh")
  val fmtMm: DateTimeFormatter = DateTimeFormat.forPattern("mm")

  def alphaNum6 = for {
    a <- Gen.alphaUpperChar.sample
    b <- Gen.numChar.sample
    c <- Gen.numChar.sample
    d <- Gen.alphaUpperChar.sample
    e <- Gen.alphaUpperChar.sample
    f <- Gen.numChar.sample
  } yield List(a, b, c, d, e, f).mkString

  def alphaNum2: String = s"${Gen.alphaUpperChar.sample.get}${Gen.numChar.sample.get}"

  /*
  * Transforamtion from MMddyyyy -> MMddyyy (dropping second digit from year)
  * */
  def toPolarDate7DigitFormat(date: String) = {
    val b = date.toBuffer
    b.remove(5)
    b.mkString
  }

  /*
  * Transforamtion from MMddyyy -> MMddyy (dropping whole century from year)
  * */
  def toPolarDate6DigitFormat(date: String) = {
    val b = date.toBuffer
    b.remove(5)
    b.remove(5)
    b.mkString
  }

  def oneOrZero = Gen.oneOf(0, 1)
  def voyageSubffixGen = Gen.oneOf("A", "B", "C", "D", "E", "F")

  def bedConfigLandGen = Gen.oneOf("T", "Q", "D", "K")
  def guestBookingNumber = Gen.function0().sample.get

  implicit val bookingNumberArb = Arbitrary(bookingNumber)

  //BookingNumber comes from DISPLAY-LOCATOR and xiLodging wants to ge a nonRepeating number
  //of exact length of 6. We take the last 6 digits of the current milliseconds.
  def bookingNumber: String = {
    def value(): String = {
      val instant = (new java.util.Date()).getTime.toString
      instant.substring(instant.length - 6, instant.length)
    }
    value
  }
  def ynGen: Gen[String] = Gen.oneOf("Y", "N", "00")
  def intGen: Gen[Int] = Gen.choose[Int](1, 100)
  def booleanGen: Gen[Boolean] = Gen.oneOf(true, false)
  var dateGen = Gen.choose(DateTime.now().plusMonths(1).getMillis, DateTime.now().plusYears(10).getMillis)
  def beforeDateGen = Gen.choose(DateTime.now.minusYears(10).getMillis, DateTime.now.minusYears(1).getMillis)
  def afterDateGen = Gen.choose(
    DateTime.now().plusMonths(1).getMillis,
    DateTime.now().plusMonths(2).getMillis
  )

  def phoneIntGen: Gen[Int] = Gen.oneOf(1234567809, 2019284759, 898765294)

  lazy val currencyCodeSource = Source.fromURL(getClass.getResource("/currency_codes.txt")).getLines().toList

  def currencyCodeGenerator = Gen.chooseNum(0, currencyCodeSource.length - 1).map(currencyCodeSource(_))

  lazy val shipCodeSource = Source.fromURL(getClass.getResource("/ship_code.txt")).getLines().toList

  def shipCodeGenerator = Gen.chooseNum(0, shipCodeSource.length - 1).map(shipCodeSource(_))

  lazy val messageCodeSource = Source.fromURL(getClass.getResource("/message_code.txt")).getLines().toList

  def messageCodeGenerator = Gen.chooseNum(0, messageCodeSource.length - 1).map(messageCodeSource(_))

  def matchStatusGen = Gen.oneOf(MatchStatus.values.toSeq)
  def cancelCodeGen = Gen.oneOf(CancelCode.values.toSeq)
  def groupTypeGen = Gen.oneOf(GroupType.values.toSeq)
  def paxTypeGen = Gen.oneOf(PaxType.values.toSeq)
  def titleGen = Gen.oneOf(Title.values.toSeq)
  def genderGen = Gen.oneOf(Gender.values.toSeq)
  def phoneTypeGen = Gen.oneOf(PhoneType.values.toSeq)


  /**
    * Genereator for ship codes. Support positive and negative cases.
    *
    * @param valid If `true` (the default) returns valid Ship ID codes, otherwise returns invalid codes.
    * @return A `Gen[String]` representing a valid or invalid ship ID code (depending on the value of `valid`).
    */
  def shipIDGenerator(valid: Boolean = true): Gen[String] = Gen.oneOf(AirlineID.values.toList).map(s => if (valid) s else Gen.oneOf(simpleRandomShortCode(4).take(3), s"${s}${simpleRandomShortCode(4)}").sample.get)
}
