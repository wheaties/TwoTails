package twotails

import org.scalatest.{ FlatSpec, Matchers }

class Bippy{
  @mutualrec final def one(x: Int, y: Int = 1): Int = if(0 < x) two(y, x) else 0
  @mutualrec final def two(x: Int, y: Int = 1): Int = if(0 < x) one(x-1, y-1) else 0
}

class Baz{
  @mutualrec final def one(x: Int)(y: Int): Int = if(0 < x) two(y)(x) else 0
  @mutualrec final def two(x: Int)(y: Int): Int = if(0 < x) one(x-1)(y-1) else 0
}

class Bazooka{
  @mutualrec final def one(x: Int)(y: Int)(z: Int): Int = if(0 < x) two(y)(x)(z) else z
  @mutualrec final def two(x: Int)(y: Int)(z: Int): Int = if(0 < x) one(x-1)(y-1)(z+1) else z
}

//This is more of a "does this compile" check. Move to Par-test.
//TODO: This fails!
/*class RocketLauncher{
  @mutualrec final def one(x: Int, y: Int*): Int = if(0 < x) two(x-1, y: _*) else 0
  @mutualrec final def two(x: Int, y: Int*): Int = if(0 < x) one(x-1, y: _*) else 0
}*/

class ArgumentListTest extends FlatSpec with Matchers{
  val fourK = 400000

  "Two mutually recursive, double-argument list, annotated methods" should "not throw a StackOverflow" in{
  	val c = new Baz

  	c.one(fourK)(fourK) should equal (0)
  }

  "Two mutually recursive, multi-argument list, annotated methods" should "not throw a StackOverflow" in{
    val baz = new Bazooka

    baz.one(fourK)(fourK)(0) should equal (fourK)
  }
}