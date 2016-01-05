package jp.dakatsuka.mahjongg.core

import org.scalacheck.Gen
import org.scalatest.{MustMatchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class PaiSpec extends PropSpec with PropertyChecks with MustMatchers {
  lazy val genManz: Gen[Pai] = Gen.oneOf(Pai.manz)
  lazy val genPinz: Gen[Pai] = Gen.oneOf(Pai.pinz)
  lazy val genSouz: Gen[Pai] = Gen.oneOf(Pai.souz)
  lazy val genKaze: Gen[Pai] = Gen.oneOf(Pai.kazehai)
  lazy val genSang: Gen[Pai] = Gen.oneOf(Pai.sangenpai)
  lazy val genRotohai: Gen[Pai] = Gen.oneOf(Pai.rotohai)
  lazy val genYaochuhai: Gen[Pai] = Gen.oneOf(Pai.yaochuhai)
  lazy val genGreenhai: Gen[Pai] = Gen.oneOf(Pai.greenhai)

  lazy val genNotRotohai: Gen[Pai] = Gen.oneOf(Pai.suhai.filterNot(p => Pai.rotohai.contains(p)))
  lazy val genNotYaochuhai: Gen[Pai] = Gen.oneOf(Pai.all.filterNot(p => Pai.yaochuhai.contains(p)))
  lazy val genNotGeeenhai: Gen[Pai] = Gen.oneOf(Pai.all.filterNot(p => Pai.greenhai.contains(p)))

  property("isManz") {
    forAll(genManz) { p => Pai.isManz(p) mustBe true  }
    forAll(genPinz) { p => Pai.isManz(p) mustBe false }
    forAll(genSouz) { p => Pai.isManz(p) mustBe false }
    forAll(genKaze) { p => Pai.isManz(p) mustBe false }
    forAll(genSang) { p => Pai.isManz(p) mustBe false }
  }

  property("isPinz") {
    forAll(genManz) { p => Pai.isPinz(p) mustBe false }
    forAll(genPinz) { p => Pai.isPinz(p) mustBe true  }
    forAll(genSouz) { p => Pai.isPinz(p) mustBe false }
    forAll(genKaze) { p => Pai.isPinz(p) mustBe false }
    forAll(genSang) { p => Pai.isPinz(p) mustBe false }
  }

  property("isSouz") {
    forAll(genManz) { p => Pai.isSouz(p) mustBe false }
    forAll(genPinz) { p => Pai.isSouz(p) mustBe false }
    forAll(genSouz) { p => Pai.isSouz(p) mustBe true  }
    forAll(genKaze) { p => Pai.isSouz(p) mustBe false }
    forAll(genSang) { p => Pai.isSouz(p) mustBe false }
  }

  property("isSuhai") {
    forAll(genManz) { p => Pai.isSuhai(p) mustBe true  }
    forAll(genPinz) { p => Pai.isSuhai(p) mustBe true  }
    forAll(genSouz) { p => Pai.isSuhai(p) mustBe true  }
    forAll(genKaze) { p => Pai.isSuhai(p) mustBe false }
    forAll(genSang) { p => Pai.isSuhai(p) mustBe false }
  }

  property("isJihai") {
    forAll(genManz) { p => Pai.isJihai(p) mustBe false }
    forAll(genPinz) { p => Pai.isJihai(p) mustBe false }
    forAll(genSouz) { p => Pai.isJihai(p) mustBe false }
    forAll(genKaze) { p => Pai.isJihai(p) mustBe true  }
    forAll(genSang) { p => Pai.isJihai(p) mustBe true  }
  }

  property("isRotohai") {
    forAll(genRotohai)    { p => Pai.isRotohai(p) mustBe true  }
    forAll(genNotRotohai) { p => Pai.isRotohai(p) mustBe false }
  }

  property("isYaochuhai") {
    forAll(genYaochuhai)    { p => Pai.isYaochuhai(p) mustBe true }
    forAll(genNotYaochuhai) { p => Pai.isYaochuhai(p) mustBe false }
  }

  property("isGreenhai") {
    forAll(genGreenhai)    { p => Pai.isGreenhai(p) mustBe true  }
    forAll(genNotGeeenhai) { p => Pai.isGreenhai(p) mustBe false }
  }

  property("getPaiFamily") {
    forAll(Gen.oneOf(Pai.all)) { p => Pai.getPaiFamily(p) mustBe a [PaiFamily] }
  }

  property("fromStringOption") {
    forAll(Gen.oneOf("M1", "M2", "M3", "Haku")) { s => Pai.fromStringOption(s) mustBe a [Some[_]] }
    forAll {s: String => Pai.fromStringOption(s) mustBe None }
  }
}
