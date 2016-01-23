package jp.dakatsuka.mahjongg.core

import jp.dakatsuka.mahjongg.core._
import org.scalacheck.Gen
import org.scalatest.{MustMatchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class MentsuSpec extends PropSpec with PropertyChecks with MustMatchers {
  val jantoGen: Gen[Janto] = Gen.oneOf(Pai.all.map(Janto.apply))
  val shuntsuGen: Gen[Shuntsu] = Gen.oneOf(Pai.all.map(Shuntsu.apply))
  val kotsuGen: Gen[Kotsu] = Gen.oneOf(Pai.all.map(Kotsu.apply))
  val kantsuGen: Gen[Kantsu] = Gen.oneOf(Pai.all.map(Kantsu.apply))

  property("toList") {
    forAll(jantoGen)   { m => Mentsu.toList(m) }
    forAll(shuntsuGen) { m => Mentsu.toList(m) }
    forAll(kotsuGen)   { m => Mentsu.toList(m) }
    forAll(kantsuGen)  { m => Mentsu.toList(m) }
  }
}
