package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

  def not(b: Boolean): Boolean = if b then false else true

  def and(left: Boolean, right: => Boolean): Boolean = if not(left) then false else right

  def or(left: Boolean, right: => Boolean): Boolean = if left then true else right

  end `Boolean Operators`

  object `Fermat Numbers` :

  val multiplication: (BigInt, BigInt) => BigInt = (multiplicand,multiplier) =>
    @tailrec
    def multiplicationTailRec(multiplicand: BigInt, multiplier: BigInt, product: BigInt): BigInt =
      if multiplier == 0 then product
      else multiplicationTailRec(multiplicand, multiplier - 1, product + multiplicand)

  multiplicationTailRec(multiplicand, multiplier, 0)

    val power: (BigInt, BigInt) => BigInt = (a, n) =>
      if n == 0 then 1
      else  multiplication(power(a, n - 1), a)

    val fermatNumber: Int => BigInt = n => power(2, power(2, n)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence` :

  val lookAndSaySequenceElement: Int => BigInt = n => {

    /*Method that works with the string representation of numbers*/
    def lookAndSaySequence(number: String): String = {

      val result = new StringBuilder                                                        // execution result value
      // The data type choice is due to methods of this data type that allow you to work with a string as with a list

      @tailrec
      def lookAndSaySequenceLoop(numberString: String, digit: Char, times: Int): String =   // @tailrec method for string conversion
        if (numberString.isEmpty) result.toString()                                         // if there is nothing left in the list-number, it's time to return the result
        else if (numberString.head != digit)                                                // if there is a different element
          result.append(times).append(digit)                                                // add to the result the number of repetitions of the digit and the digit itself
          lookAndSaySequenceLoop(numberString.tail, numberString.head, 1)                   // call the method on the next different difit
        else lookAndSaySequenceLoop(numberString.tail, numberString.head, times + 1)        // if there is no a different elements, call the method on the next same digit,
      // increasing the counter of times

      lookAndSaySequenceLoop(number.tail + " ", number.head, 1)                              // call the loop from the lookAndSaySequence method body

    }

    @tailrec
    def lookAndSaySequenceElementCounter(n: Int, num: String): BigInt = {                  // @tailrec method for recursive calculation of the value of the n-th term of the sequence
      if (n <= 0) BigInt (num)
      else lookAndSaySequenceElementCounter (n - 1, lookAndSaySequence (num) )
    }

    lookAndSaySequenceElementCounter(n, "1")                                                // call for counting the nth member of the sequence, specifying the value of the 0th as "1"

  }

  end `Look-and-say Sequence`
end Homework