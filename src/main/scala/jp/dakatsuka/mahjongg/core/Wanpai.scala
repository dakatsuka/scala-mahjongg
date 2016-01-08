package jp.dakatsuka.mahjongg.core

case class Wanpai(
  rinshanhai: List[Pai],
  dora: List[Pai],
  uradora: List[Pai],
  currentDora: List[Pai],
  currentUradora: List[Pai]
)

object Wanpai {
  def initialize(pais: List[Pai]): Wanpai = {
    val (rinshanhai, tail) = pais.splitAt(4)
    val (dora, uradora)    = tail.splitAt(5)

    Wanpai(
      rinshanhai = rinshanhai,
      dora = dora,
      uradora = uradora,
      currentDora = dora.take(1),
      currentUradora = uradora.take(1)
    )
  }
}
