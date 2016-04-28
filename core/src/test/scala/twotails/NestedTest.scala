package twotails

import org.scalatest.{ FlatSpec, Matchers }

class Nested{
  @mutualrec final def nest(z: Int): Int ={
    @mutualrec def one(x: Int): Int = if(0 < x) two(x-1) else 0
    @mutualrec def two(x: Int): Int = if(0 < x) one(x-2) else 0

    if(0 < z) one(z) else aha(z)
  }

  @mutualrec final def aha(z: Int): Int = nest(z+1)

  def thing(y: Int) ={
    class Yo{
      @mutualrec final def one(x: Int): Int = if(0 < x) two(x-1) else 0
      @mutualrec final def two(x: Int): Int = if(0 < x) one(x-2) else 0
    }

    (new Yo).two(y)
  }

  def other(y: Int) ={
    object Yo{
      @mutualrec final def one(x: Int): Int = if(0 < x) two(x-1) else 0
      @mutualrec final def two(x: Int): Int = if(0 < x) one(x-2) else 0
    }

    Yo.one(y)
  }

  class Foo{
    @mutualrec final def one(x: Int): Int = if(0 < x) two(x-1) else 0
    @mutualrec final def two(x: Int): Int = if(0 < x) one(x-2) else 0
  }

  def something(y: Int): Int ={
  	@mutualrec def one(x: Int): Int = if(0 < x) two(x-1) else 0
    @mutualrec def two(x: Int): Int = if(0 < x) one(x-2) else 0

    one(y)
  }

  { //just a block which will be discarded
  	@mutualrec def one(x: Int): Int = if(0 < x) two(x-1) else 0
    @mutualrec def two(x: Int): Int = if(0 < x) one(x-2) else 0
  }

  def dis(y: Int) ={
  	{ //another block which will be discarded but with a name clash
  	  @mutualrec def one(x: Int): Int = if(0 < x) two(x-1) else 1
      @mutualrec def two(x: Int): Int = if(0 < x) one(x-2) else 1
    }

    @mutualrec def one(x: Int): Int = if(0 < x) two(x-1) else 0
    @mutualrec def two(x: Int): Int = if(0 < x) one(x-2) else 0

    one(y)
  }

  val that ={ xy: Int =>
  	@mutualrec def one(x: Int): Int = if(0 < x) two(x-1) else 0
    @mutualrec def two(x: Int): Int = if(0 < x) one(x-2) else 0

    one(xy)
  }
}

class NestedTest extends FlatSpec with Matchers{
  val fourK = 400000

  "A nested class within a def which has annotated methods" should "not throw a StackOverflow" in{
  	val nest = new Nested

  	nest.thing(fourK) should equal (0)
  }

  "A nested object within a def which has annotated methods" should "not throw a StackOverflow" in{
  	val nest = new Nested

  	nest.other(fourK) should equal (0)
  }

  "A nested class within a class which has annotated methods" should "not throw a StackOverflow" in{
  	val nest = new Nested
  	val foo = new nest.Foo

  	foo.one(fourK) should equal (0)
  }
}