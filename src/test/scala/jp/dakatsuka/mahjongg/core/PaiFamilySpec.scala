package jp.dakatsuka.mahjongg.core

import org.scalacheck.Gen
import org.scalatest.{MustMatchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class PaiFamilySpec extends PropSpec with PropertyChecks with MustMatchers {
  property("fromStringOption") {
    forAll(Gen.oneOf("Manz", "Pinz", "Souz", "Kazehai", "Sangenpai")) { s =>
      PaiFamily.fromStringOption(s) mustBe a [Some[_]]
    }

    forAll { s: String => PaiFamily.fromStringOption(s) mustBe None }
  }
}
