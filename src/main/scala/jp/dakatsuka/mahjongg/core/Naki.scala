package jp.dakatsuka.mahjongg.core

sealed trait Naki
case class Pon(kotsu: Kotsu, position: Position) extends Naki
case class Chi(shuntsu: Shuntsu, nakihai: Pai) extends Naki
case class Minkan(kantsu: Kantsu, position: Position) extends Naki
case class Ankan(kantsu: Kantsu) extends Naki
