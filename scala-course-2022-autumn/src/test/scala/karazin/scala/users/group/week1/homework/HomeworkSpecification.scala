package karazin.scala.users.group.week1.homework

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Homework._
import karazin.scala.users.group.week1.homework.arbitraries

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndAaSequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators"):
  import `Boolean Operators`._

  property("not") = forAll { (b: Boolean) => not(b) == !b }

  property("and") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    (and(left, right) == (left && right)) && (and(false,(7/0 == 2)) == false)
  }

  property("and(error)") = propBoolean {
    val error = new Exeption("And(error)'s exeption")
    and(false, throw error) == false
  }

  property("or") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    (or(left, right) == (left || right)) && (or(true,(7/0 == 2)) == true)
  }

  property("or(error)") = propBoolean {
  val error = new Exeption("Or(error)'s exeption")
  or(true, throw error) == true
  }

end BooleanOperatorsSpecification

object FermatNumbersSpecification extends Properties("Fermat Numbers"):
  import `Fermat Numbers`._
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: Int, right: Int) => multiplication(left, right) == (left * right) }

  property("power") = forAll { (left: Int, right: Int) => power(left, right) == (0 until right).foldLeft(BigInt(1)) { (acc, _) => acc * left }

  property("fermatNumber") = forAll { (n: Int) => fermatNumber(n) == Math.pow(2, Math.pow(2, n)) + 1 }

end FermatNumbersSpecification

object LookAndAaSequenceSpecification extends Properties("Look-and-say Sequence"):
  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]

  var z = new Array[BigInt](10)
  z = Array(1, 11, 21, 1211, 111221, 312211, 13112221, 1113213211, 31131211131221, 13211311123113112211)

  property("Look-and-say Sequence" ) = forAll { (n: Int) =>
  if n < z.length then lookAndSaySequenceElement(n) == z[n]
  else !lookAndSaySequenceElement(n).toString().contains("4")

  }  

end LookAndAaSequenceSpecification
