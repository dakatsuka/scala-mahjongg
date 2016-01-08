package jp.dakatsuka.mahjongg.core

import org.scalacheck.Gen
import org.scalatest.{PropSpec, MustMatchers}
import org.scalatest.prop.PropertyChecks

class WanpaiSpec extends PropSpec with PropertyChecks with MustMatchers {
  val genPai: Gen[Pai] = Gen.oneOf(Pai.all)
  val genPaiList: Gen[List[Pai]] = Gen.listOfN(14, genPai)

  property("initialize") {
    forAll(genPaiList) { pais => Wanpai.initialize(pais).rinshanhai.length mustEqual 4 }
    forAll(genPaiList) { pais => Wanpai.initialize(pais).dora.length mustEqual 5 }
    forAll(genPaiList) { pais => Wanpai.initialize(pais).uradora.length mustEqual 5 }
    forAll(genPaiList) { pais =>
      val wanpai = Wanpai.initialize(pais)
      wanpai.currentDora mustEqual wanpai.dora.take(1)
      wanpai.currentUradora mustEqual wanpai.uradora.take(1)
    }
  }
}
