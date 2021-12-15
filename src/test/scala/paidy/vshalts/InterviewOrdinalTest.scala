package paidy.vshalts

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import Interview._

class InterviewOrdinalTest extends AnyFunSuite with should.Matchers {
  test("Convert English ordinal numbers for *1,*2,*3") {
    toOrdinalString(1) shouldBe "1st"
    toOrdinalString(2) shouldBe "2nd"
    toOrdinalString(3) shouldBe "3rd"
    toOrdinalString(21) shouldBe "21st"
    toOrdinalString(32) shouldBe "32nd"
    toOrdinalString(43) shouldBe "43rd"
    toOrdinalString(131) shouldBe "131st"
  }

  test("Convert English ordinal numbers for teens") {
    for (i <- 10 until 20)
      toOrdinalString(i) shouldBe s"${i}th"
  }

  test("Convert English ordinal numbers for the rest of cases") {
    toOrdinalString(0) shouldBe "0th"
    toOrdinalString(4) shouldBe "4th"
    toOrdinalString(5) shouldBe "5th"
    toOrdinalString(9) shouldBe "9th"
    toOrdinalString(24) shouldBe "24th"
    toOrdinalString(154) shouldBe "154th"
  }
}
