package twotails

import org.scalatest.{ FlatSpec, Matchers }
import annotation.tailrec
import java.lang.StackOverflowError

class Foo{
  @mutualrec final def yo(x: Int): Int = if(0 < x) yo(x-1) else 0
}

class Foo2{
  @tailrec final def yo(x: Int): Int = if(0 < x) yo(x-1) else 0
}

class Bar{
  @mutualrec def one(x: Int): Int = if(0 < x) two(x-1) else x
  @mutualrec def two(x: Int): Int = if(0 < x) one(x-2) else x
}

class ErrBar{
  def one(x: Int): Int = if(0 < x) two(x-1) else 0
  def two(x: Int): Int = if(0 < x) one(x-2) else 0
}

/*class Baz{
  @mutualrec def one(x: Int)(y: Int): Int = if(x < 0) two(y)(x) else 0
  @mutualrec def two(x: Int)(y: Int): Int = if(x < 0) one(x-1)(y-1) else 0
}*/

class Bippy{
  @mutualrec def one(x: Int, y: Int = 1): Int = if(0 < x) two(y, x) else 0
  @mutualrec def two(x: Int, y: Int = 1): Int = if(0 < x) one(x-1, y-1) else 0
}

object Blappy{
  @mutualrec def one(x: Int): Int = if(0 < x) two(x-1) else 0
  @mutualrec def two(x: Int): Int = if(0 < x) one(x-2) else 0
}

class Nested{
  def thing(y: Int) ={
    class Yo{
      @mutualrec def one(x: Int): Int = if(0 < x) two(x-1) else 0
      @mutualrec def two(x: Int): Int = if(0 < x) one(x-2) else 0
    }

    (new Yo).two(y)
  }
}

class MutualRecTest extends FlatSpec with Matchers{
	val seventyK = 70000
	val fourK = 400000
  
  "A single argument, annotated method" should "be equivalent to a tailrec" in{
    val foo = new Foo
    val foo2 = new Foo2

    foo.yo(seventyK) should equal {
      foo2.yo(seventyK)
    }
  }

  "Two mutually recursive, single argument, annotated methods" should "not throw a StackOverflow" in{
    val b = new Bar

    b.one(fourK) should equal (0)
  }

  "Two mutually recursive, single argument but not annotated methods" should "throw a StackOverflow" in{
    val err = new ErrBar

    intercept[StackOverflowError]{
      err.one(fourK)
    }
  }

  "Two mutually recursive, multi-argument, annotated methods" should "not throw a StackOverflow" in{
  	val c = new Bippy

  	c.one(fourK, fourK) should equal (0)
  }

  "Two mutually recursive, single argument, annotated methods on an object" should "not throw a StackOverflow" in{
    Blappy.one(fourK) should equal (0)
  }

  "A nested class within a def which has annotated methods" should "not throw a StackOverflow" in{
  	val nest = new Nested

  	nest.thing(fourK) should equal(0)
  }
}