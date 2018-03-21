package io.bytetrend

import java.time.LocalDate

object Constants {

  final val MinimumGuestDetailPerBooking = 1
  final val MaximumGuestDetailPerBooking = 5
  final val TwoEvents = 2

  final val MinimumLatency = 1
  final val LowLatency = MinimumLatency * 5
  final val MediumLatency = MinimumLatency * 10
  final val HighLatency = MinimumLatency * 15

  final val negativeTwenty = -20

  final val one = 1
  final val eight = 8
  final val twelve = 12
  final val fifteen = 15

  final val oneYear = 1
  final val thousandHours = 1000
  final val hundredDays = 100
  final val hundredHours = 100
  final val tenDays = 10

  val AirlineID = Map[String, String](
    "UA" -> "United",
    "DL" -> "Delta",
    "JB" -> "JetBlue",
    "AV" -> "Avianca"
  )
}
object MatchStatus extends Enumeration {
  val ` `, A, O, M, S = Value
}

case class Waitlist(
                     originalBookingStatus: Option[String],
                     originalBookingCategory: Option[String],
                     originalBookingCabin: Option[String],
                     originalBookingCell: Option[String],
                     matchStatus: MatchStatus.Value
                   )

object CancelCode extends Enumeration {
  val A, M, X, U, I = Value
}

case class Cancellation(cancelDate: Option[LocalDate], cancelCode: Option[CancelCode.Value])

case class AccommodationInfo(roomNumber: Option[String], categoryCode: Option[String])

object GroupType extends Enumeration {
  val F, G, A, H, I, N, O, T, W = Value
}

case class Group(group: Option[String], groupType: Option[GroupType.Value])


object PaxType extends Enumeration {
  val FIT, Group, Promo, Concession, UKTotal, Australia, Charter, `OtherLocation` = Value
}

object Title extends Enumeration {
  val Mr, Mrs, Ms = Value
}

object Gender extends Enumeration {
  val Male, Female = Value
}

object PhoneType extends Enumeration {
  val land, mobile = Value
}

