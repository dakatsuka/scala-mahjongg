package jp.dakatsuka.mahjongg.core

sealed abstract class Pai(val index: Int, val character: String) extends Ordering[Pai] with Ordered[Pai] {
  def compare(that: Pai): Int = index compare that.index
  def compare(x: Pai, y: Pai): Int = x.index compare y.index
}

object Pai {
  case object M1 extends Pai(index = 0, character = "\uD83C\uDC07")
  case object M2 extends Pai(index = 1, character = "\uD83C\uDC08")
  case object M3 extends Pai(index = 2, character = "\uD83C\uDC09")
  case object M4 extends Pai(index = 3, character = "\uD83C\uDC0A")
  case object M5 extends Pai(index = 4, character = "\uD83C\uDC0B")
  case object M6 extends Pai(index = 5, character = "\uD83C\uDC0C")
  case object M7 extends Pai(index = 6, character = "\uD83C\uDC0D")
  case object M8 extends Pai(index = 7, character = "\uD83C\uDC0E")
  case object M9 extends Pai(index = 8, character = "\uD83C\uDC0F")
  case object P1 extends Pai(index = 9, character = "\uD83C\uDC19")
  case object P2 extends Pai(index = 10, character = "\uD83C\uDC1A")
  case object P3 extends Pai(index = 11, character = "\uD83C\uDC1B")
  case object P4 extends Pai(index = 12, character = "\uD83C\uDC1C")
  case object P5 extends Pai(index = 13, character = "\uD83C\uDC1D")
  case object P6 extends Pai(index = 14, character = "\uD83C\uDC1E")
  case object P7 extends Pai(index = 15, character = "\uD83C\uDC1F")
  case object P8 extends Pai(index = 16, character = "\uD83C\uDC20")
  case object P9 extends Pai(index = 17, character = "\uD83C\uDC21")
  case object S1 extends Pai(index = 18, character = "\uD83C\uDC10")
  case object S2 extends Pai(index = 19, character = "\uD83C\uDC11")
  case object S3 extends Pai(index = 20, character = "\uD83C\uDC12")
  case object S4 extends Pai(index = 21, character = "\uD83C\uDC13")
  case object S5 extends Pai(index = 22, character = "\uD83C\uDC14")
  case object S6 extends Pai(index = 23, character = "\uD83C\uDC15")
  case object S7 extends Pai(index = 24, character = "\uD83C\uDC16")
  case object S8 extends Pai(index = 25, character = "\uD83C\uDC17")
  case object S9 extends Pai(index = 26, character = "\uD83C\uDC18")
  case object Ton extends Pai(index = 27, character = "\uD83C\uDC00")
  case object Nan extends Pai(index = 28, character = "\uD83C\uDC01")
  case object Sha extends Pai(index = 29, character = "\uD83C\uDC02")
  case object Pei extends Pai(index = 30, character = "\uD83C\uDC03")
  case object Haku extends Pai(index = 31, character = "\uD83C\uDC06")
  case object Hatsu extends Pai(index = 32, character = "\uD83C\uDC05")
  case object Chun extends Pai(index = 33, character = "\uD83C\uDC04")

  val manz = List(M1, M2, M3, M4, M5, M6, M7, M8, M9)
  val pinz = List(P1, P2, P3, P4, P5, P6, P7, P8, P9)
  val souz = List(S1, S2, S3, S4, S5, S6, S7, S8, S9)
  val kazehai = List(Ton, Nan, Sha, Pei)
  val sangenpai = List(Haku, Hatsu, Chun)
  val suhai = manz ::: pinz ::: souz
  val jihai = kazehai ::: sangenpai
  val rotohai = List(M1, M9, P1, P9, S1, S9)
  val greenhai = List(S2, S3, S4, S6, S8, Hatsu)
  val yaochuhai = rotohai ::: jihai
  val all = suhai ::: jihai

  val suhaiCount = suhai.length
  val jihaiCount = jihai.length
  val paiToalCount = suhaiCount + jihaiCount

  def isManz(pai: Pai): Boolean = manz.contains(pai)
  def isPinz(pai: Pai): Boolean = pinz.contains(pai)
  def isSouz(pai: Pai): Boolean = souz.contains(pai)
  def isKaze(pai: Pai): Boolean = kazehai.contains(pai)
  def isSang(pai: Pai): Boolean = sangenpai.contains(pai)
  def isSuhai(pai: Pai): Boolean = suhai.contains(pai)
  def isJihai(pai: Pai): Boolean = jihai.contains(pai)
  def isRotohai(pai: Pai): Boolean = rotohai.contains(pai)
  def isYaochuhai(pai: Pai): Boolean = yaochuhai.contains(pai)
  def isGreenhai(pai: Pai): Boolean = greenhai.contains(pai)

  def getPaiFamily(pai: Pai): PaiFamily = pai match {
    case M1 | M2 | M3 | M4 | M5 | M6 | M7 | M8 | M9 => PaiFamily.Manz
    case P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9 => PaiFamily.Pinz
    case S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 => PaiFamily.Souz
    case Ton | Nan | Sha | Pei                      => PaiFamily.Kazehai
    case Haku | Hatsu | Chun                        => PaiFamily.Sangenpai
  }

  def fromStringOption(value: String): Option[Pai] =
    all.toVector.find(_.toString == value)

  def makeShuntsu(pai: Pai): List[Pai] = getPaiFamily(pai) match {
    case PaiFamily.Manz if pai < M8 => manz.filter(_.index >= pai.index).take(3)
    case PaiFamily.Pinz if pai < P8 => pinz.filter(_.index >= pai.index).take(3)
    case PaiFamily.Souz if pai < S8 => souz.filter(_.index >= pai.index).take(3)
    case _ => Nil
  }
}
