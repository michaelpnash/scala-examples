package matrix

import org.scalatest.FunSpec

class MatrixTests extends FunSpec {
	def cartesianProduct(original: List[List[_]]): List[List[_]] =
	    original match {
	      case one :: two :: Nil =>
	        one.map(left => two.map(right => List(left, right))).flatten
	      case one :: two :: cc =>
	        cartesianProduct(two :: cc).map(li => one.map(left => left :: li)).flatten
	      case _ => original
	    }

	describe("the cartesian product method") {
		it ("will produce the empty list given an empty list as input") {
			assert(cartesianProduct(List()) === List())
		}
		it ("will produce a list with one element given a list with one element") {
			val l = List(List("thing"))
			assert(cartesianProduct(l) === l)
		}
		it ("will produce a list with the same elements given a single list of elements") {
			val l = List(List("thing 1", "thing2"))
			assert(cartesianProduct(l) === l)
		}
		it ("will produce a cartesian product given more than one list") {
			val l = List(List("thing 1", "thing 2"), List("other 1", "other 2"))
			assert(cartesianProduct(l) === List(List("thing 1", "other 1"), List("thing 1", "other 2"), List("thing 2", "other 1"), List("thing 2", "other 2")))
		}
		it ("will produce a cartesian product of ints given multiple lists of ints") {
			val l = List(List(1, 2), List(3, 4))
			assert(cartesianProduct(l) === List(List(1, 3), List(1, 4), List(2, 3), List(2, 4)))
		}
		it ("will produce a cartesian product with non-equal-sized lists") {
			val l = (List(List(1, 2, 3), List(2, 3)))
			assert(cartesianProduct(l) === List(List(1, 2), List(1, 3), List(2, 2), List(2, 3), List(3, 2), List(3, 3)))
		}
		it ("will produce a cartesian product with more than two lists") {
			val l = List(List(1, 2), List(3, 4), List(5))
			assert(cartesianProduct(l) === List(List(1, 3, 5), List(2, 3, 5), List(1, 4, 5), List(2, 4, 5)))
		}

	}
}