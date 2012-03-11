package dates

import java.util.Date
import java.text.{ParseException, SimpleDateFormat, DateFormat}

class DateConvert(val origin: String) {
  val formats = List(DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT), new SimpleDateFormat("yyyy/MM/dd"), new SimpleDateFormat("MM/dd/yyyy"))

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
      //setup
      val expected = new GregorianCalendar(2010, JANUARY, 1).getTime
      val origin = "2010/01/01"
      val converter = new DateConvert(origin)
      //execute
      val actual = converter.asDate
      //assert
      assert(actual.getTime === expected.getTime)
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
  }
}
