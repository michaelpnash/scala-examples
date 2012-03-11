package dates

import java.util.Date
import java.text.{ParseException, SimpleDateFormat}

class DateConvert(val origin: String) {
  val formats = List(new SimpleDateFormat("MMM dd, yyyy"), new SimpleDateFormat("yyyy/MM/dd"), new SimpleDateFormat("MM/dd/yyyy"))

  def asDate: Date = {
    if (origin == null) return null
    formats.foreach(format => {
      try {
        val proposedDate = format.parse(origin)
        if (proposedDate.getTime > 0) return proposedDate
      } catch {
        case pe: ParseException => {} //purposely ignored, we just try the next format
      }
    })
    throw new IllegalArgumentException("Unable to parse " + origin + " into a date")
  }
}

import org.scalatest.FunSpec
import java.util.GregorianCalendar
import java.util.Calendar._

class DateConversionsTests extends FunSpec {
  describe("the date convert class") {
    it ("can convert a string into the corresponding date explicitly") {
      val expected = new GregorianCalendar(2010, JANUARY, 1).getTime
      val origin = "2010/01/01"
      val converter = new DateConvert(origin)
      val actual = converter.asDate
      assert(actual.getTime === expected.getTime)
    }
    it ("can convert a string in several different formats into a date") {
      val expected = new GregorianCalendar(2010, JANUARY, 1).getTime
      assert(new DateConvert("01/01/2010").asDate.getTime === expected.getTime)
      assert(new DateConvert("Jan 1, 2010").asDate.getTime === expected.getTime)
    }
    it ("will throw an exception if the string cannot be converted to a date") {
      intercept[IllegalArgumentException] {
        new DateConvert("foo").asDate
      }
    }
    it ("can be used to implicitly convert a string to a DateConvert") {
      implicit def wrapString(s: String) = new DateConvert(s)
      val expected = new GregorianCalendar(2010, JANUARY, 1).getTime
      val origin = "2010/01/01"
      assert(origin.asDate.getTime === expected.getTime)
    }
    it ("can be used to implicitly convert a string to a date") {
      implicit def wrapString(s: String) = new DateConvert(s).asDate
      def getTimeFromDate(date: Date) = date.getTime
      val expected = new GregorianCalendar(2010, JANUARY, 1).getTime
      assert(getTimeFromDate("2010/01/01") === expected.getTime)
    }
  }
}
