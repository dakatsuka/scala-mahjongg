package jp.dakatsuka.mahjongg.core

sealed trait Mentsu {
  val p: Pai
}

case class Janto(p: Pai) extends Mentsu
case class Shuntsu(p: Pai) extends Mentsu
case class Kotsu(p: Pai) extends Mentsu
case class Kantsu(p: Pai) extends Mentsu

object Mentsu {
  def toList(m: Mentsu): List[Pai] = m match {
    case Janto(p)    => List(p, p)
    case Shuntsu(p)  => Pai.makeShuntsu(p)
    case Kotsu(p)    => List(p, p, p)
    case Kantsu(p)   => List(p, p, p, p)
  }
}
