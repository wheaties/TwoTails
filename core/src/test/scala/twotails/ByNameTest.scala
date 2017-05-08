package twotails

import org.scalatest.{ FlatSpec, Matchers }

class YelloJacket{
  @mutualrec final def one(x: Int, y: => Int): Int = if(0 < x) two(x-1, y) else y
  @mutualrec final def two(x: Int, y: => Int): Int = if(0 < x) one(x-1, y) else y
}

class Wasp{
  @mutualrec final def one(x: Int, y: => Int): Int = if(0 < x) two(x-y, 1) else y
  @mutualrec final def two(x: Int, y: => Int): Int = if(0 < x) one(x-y, 1) else y
}

//TODO: This really is more of a "does this compile" thing. Move to Partest.
class Hornet{
  private def act[A](f: => A): A = f

  @mutualrec final def one(x: Int, y: => Int): Int = if(0 < x) two(x-1, y) else act(y)
  @mutualrec final def two(x: Int, y: => Int): Int = if(0 < x) one(x-1, y) else act(y)
}

//TODO: This really is more of a "does this compile" thing. Move to Partest.
class BumbleBee{
  private def act[A](f: () => A): A = f()

  @mutualrec final def one(x: Int, y: => Int): Int = if(0 < x) two(x-1, y) else act(y _)
  @mutualrec final def two(x: Int, y: => Int): Int = if(0 < x) one(x-1, y) else act{ () => y }
}

class ByNameTest extends FlatSpec with Matchers{
  val fourK = 400000

  "mutually recursive functions with byname parameters" should "just work" in{
    val yellow = new YelloJacket

    yellow.one(fourK, 5) should equal(5)
  }
}