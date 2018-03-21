package io.bytetrend
import java.time.{LocalDateTime, LocalDate => JLocalDate, LocalTime => JLocalTime}

import io.bytetrend.Constants._
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.scalacheck.Gen

object DataFactory {

  def strings: Gen[String] = Gen.oneOf("naewwe", "afdafa", "fdafds", "fdafds")

  def strings3: Gen[String] = Gen.oneOf("nae", "afa", "fda", "faf")

  def strings4: Gen[String] = Gen.oneOf("na1e", "a2fa", "f3da", "f4af")

  def strings6: Gen[String] = Gen.oneOf("fffdsa", "eadfes", "qwfhde", "bvgeds")

  def emptyString: Gen[String] = Gen.oneOf("", "")

  def pastDatetime: Gen[DateTime] = Gen.oneOf(DateTime.now.minusYears(1), DateTime.now.minusDays(3))

  def pastDateTime: Gen[java.time.LocalDate] = Gen.oneOf(java.time.LocalDate.now.minusYears(1), java.time.LocalDate.now.minusDays(3))

  def pastLocalDate: Gen[LocalDate] = Gen.oneOf(LocalDate.now.minusYears(1), LocalDate.now.minusDays(3))

  def pastJavaLocalDate: Gen[java.time.LocalDate] = Gen.oneOf(java.time.LocalDate.now.minusYears(1), java.time.LocalDate.now.minusDays(3))

  def pastLocalTime: Gen[LocalTime] = Gen.oneOf(LocalTime.now.minusHours(1000), LocalTime.now.minusHours(100))

  def pastLocalDateTime: Gen[java.time.LocalDateTime] = Gen.oneOf(java.time.LocalDateTime.now.minusYears(oneYear), java.time.LocalDateTime.now.minusDays(3))

  def futureDatetime: Gen[DateTime] = Gen.oneOf(DateTime.now.plusYears(1), DateTime.now.plusDays(3))

  def futureJavaLocalDate: Gen[java.time.LocalDate] = Gen.oneOf(java.time.LocalDate.now.plusYears(1), java.time.LocalDate.now.plusDays(3))

  def futureLocalTime: Gen[LocalTime] = Gen.oneOf(LocalTime.now.plusHours(1000), LocalTime.now.plusHours(100))

  def futureLocalDate: Gen[LocalDate] = Gen.oneOf(LocalDate.now.plusDays(1), LocalDate.now.plusDays(100))

  def futureLocalDateTime: Gen[LocalDateTime] = Gen.oneOf(LocalDateTime.now.plusHours(thousandHours), LocalDateTime.now.plusHours(hundredHours))

  def floats: Gen[Float] = Gen.oneOf(2F, 1F, 9F, 3F)

  def booleans: Gen[Boolean] = Gen.oneOf(true, false)

  val ints: Gen[Int] = Gen.choose(1, 90)

  def negativeInts: Gen[Int] = Gen.choose(-1, -10000)

  val intstrs: Gen[String] = Gen.oneOf("123", "345", "567", "5555", "12345568990")

  def randomIndex(i: Int): Gen[Int] = Gen.choose(0, 10000000 % i)

}
