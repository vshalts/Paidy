package paidy.vshalts

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import paidy.vshalts.Interview._

class InterviewObfuscateTest extends AnyFunSuite with should.Matchers {
  test("Obfuscate can parse email") {
    obfuscate("local-part@domain-name.com") shouldEqual "l*****t@domain-name.com"
  }

  test("Corner case email with 1 character account") {
    obfuscate("l@domain-name.com") shouldEqual "l*****@domain-name.com"
  }

  test("email can't have two @") {
    intercept[IllegalArgumentException] {
      obfuscate("local-part@@domain-name.com")
    }
  }

  test("email should have correct domain format") {
    // not checking for all invalid characters here, only then regexp can handle it in general
    intercept[IllegalArgumentException] {
      obfuscate("local-part@@domain$name.com")
    }
  }

  test("phone should have a least 9 digits") {
    intercept[IllegalArgumentException] {
      obfuscate("+44 123 456")
    }
  }

  test("phone can have only one +") {
    intercept[IllegalArgumentException] {
      obfuscate("++44 123 456")
    }
  }

  test("phone can be obfuscated") {
    obfuscate("+44 123 456 789") shouldEqual "+**-***-**6-789"
  }

  test("plus not required for phone") {
    obfuscate("44 123 456 789") shouldEqual "**-***-**6-789"
  }

  test("phone spaces are optional") {
    obfuscate("+44 123456789") shouldEqual "+**-*****6789"
  }

  test("phone with no spaces is valid format") {
    obfuscate("+44123456789") shouldEqual "+*******6789"
  }

  test("phone with just digits is valid") {
    obfuscate("44123456789") shouldEqual "*******6789"
  }
}
