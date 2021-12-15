package paidy.vshalts

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should
import paidy.vshalts.Interview._

class InterviewSundaysTest extends AnyFunSuite with should.Matchers {
  test("Correct result for the provided example") {
    sundaysBetween("01-05-2021", "30-05-2021") shouldEqual 5
  }

  test("No sunday in between") {
    sundaysBetween("04-05-2021", "07-05-2021") shouldEqual 0
  }

  test("Start from Sunday") {
    sundaysBetween("02-05-2021", "07-05-2021") shouldEqual 1
  }

  test("End on Sunday") {
    sundaysBetween("03-05-2021", "23-05-2021") shouldEqual 3
  }

  test("Both date are Sundays") {
    sundaysBetween("02-05-2021", "23-05-2021") shouldEqual 4
  }

  test("Both date are Sundays 2") {
    sundaysBetween("02-05-2021", "02-05-2021") shouldEqual 1
  }
}
