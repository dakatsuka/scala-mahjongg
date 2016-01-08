package jp.dakatsuka.mahjongg.core

import scala.util.Random

case class Yama(pipai: List[Pai], wanpai: Wanpai)

object Yama {
  def initialize: Yama = {
    val shuffledPais = Random.shuffle((1 to 4).foldLeft(List.empty[Pai]) { (xs, _) => xs ::: Pai.all })

    Yama(
      pipai = shuffledPais.drop(14),
      wanpai = Wanpai.initialize(shuffledPais.take(14))
    )
  }
}
