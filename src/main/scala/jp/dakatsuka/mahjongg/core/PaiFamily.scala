package jp.dakatsuka.mahjongg.core

sealed trait PaiFamily

object PaiFamily {
  case object Manz extends PaiFamily
  case object Pinz extends PaiFamily
  case object Souz extends PaiFamily
  case object Kazehai extends PaiFamily
  case object Sangenpai extends PaiFamily

  def fromStringOption(value: String): Option[PaiFamily] =
    Vector(Manz, Pinz, Souz, Kazehai, Sangenpai).find(_.toString == value)
}
