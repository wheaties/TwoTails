package twotails

import org.scalatest.{ FlatSpec, Matchers }
import java.lang.StackOverflowError

object Blappy{
  @mutualrec final def one(x: Int): Int = if(0 < x) two(x-1) else 0
  @mutualrec final def two(x: Int): Int = if(0 < x) one(x-2) else 0

  final def three(x: Int): Int = if(0 < x) four(x-1) else 0
  final def four(x: Int): Int = if(0 < x) three(x-1) else 0
}

class ObjectTest extends FlatSpec with Matchers{
  val fourK = 400000

  "Two mutually recursive, single argument, annotated methods on an object" should "not throw a StackOverflow" in{
    Blappy.one(fourK) should equal (0)
  }

  "Two mutually recursive, single argument but not annotated methods on an object" should "throw a StackOverflow" in{
    intercept[StackOverflowError]{
      Blappy.three(fourK)
    }
  }
}