package twotails

import org.scalatest.{ FlatSpec, Matchers }
import java.lang.StackOverflowError

object Blappy{
  @mutualrec final def one(x: Int): Int = if(0 < x) two(x-1) else 0
  @mutualrec final def two(x: Int): Int = if(0 < x) one(x-1) else 0
}

class ObjectTest extends FlatSpec with Matchers{
  val fourK = 400000

  "Two mutually recursive, single argument, annotated methods on an object" should "not throw a StackOverflow" in{
    Blappy.one(fourK) should equal (0)
  }
}