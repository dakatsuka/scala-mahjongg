package jp.dakatsuka.mahjongg.core

import org.scalatest.{MustMatchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class YamaSpec extends PropSpec with PropertyChecks with MustMatchers {
  property("initialize") {
    val yama = Yama.initialize
    yama mustBe a [Yama]
    yama.pipai.length mustEqual 122
  }
}
