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
  @mutualrec final def one(x: Int): Int = if(0 < x) two(x-1) else x
  @mutualrec final def two(x: Int): Int = if(0 < x) one(x-2) else x
}

class ErrBar{
  def one(x: Int): Int = if(0 < x) two(x-1) else 0
  def two(x: Int): Int = if(0 < x) one(x-2) else 0
}

class Bippy{
  @mutualrec final def one(x: Int, y: Int = 1): Int = if(0 < x) two(y, x) else 0
  @mutualrec final def two(x: Int, y: Int = 1): Int = if(0 < x) one(x-1, y-1) else 0
}

/*class Bumpy{
  @mutualrec final def one(x: Int, y: Int = 1): Int = if(0 < x) two(x-1) else y
  @mutualrec final def two(x: Int, y: Int = 2): Int = if(0 < x) one(x-1) else y
}*/

/*class Baz{
  @mutualrec def one(x: Int)(y: Int): Int = if(x < 0) two(y)(x) else 0
  @mutualrec def two(x: Int)(y: Int): Int = if(x < 0) one(x-1)(y-1) else 0
}*/

class ArgumentListTest extends FlatSpec with Matchers{
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
}

//------start a new file here too

object Blappy{
  @mutualrec final def one(x: Int): Int = if(0 < x) two(x-1) else 0
  @mutualrec final def two(x: Int): Int = if(0 < x) one(x-2) else 0
}

class Nested{
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

  val that ={ xy: Int =>
  	@mutualrec def one(x: Int): Int = if(0 < x) two(x-1) else 0
    @mutualrec def two(x: Int): Int = if(0 < x) one(x-2) else 0

    one(xy)
  }
}

class ConstructionTest extends FlatSpec with Matchers{
  val fourK = 400000

  "Two mutually recursive, single argument, annotated methods on an object" should "not throw a StackOverflow" in{
    Blappy.one(fourK) should equal (0)
  }

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

//-----probably good place for new file

//first test arg types in the class constructor
class Fiji[A]{
  @mutualrec final def thing(x: A, y: Int): A = if(0 < y) thing(x, y-1) else x
}

class Bahama[A]{
  @mutualrec final def one(x: A, y: Int): A = if(0 < y) two(x, y-1) else x
  @mutualrec final def two(x: A, y: Int): A = if(0 < y) one(x, y-1) else x
}

class KeyLargo{
  @mutualrec final def thing[A](x: A, y: Int): A = if(0 < y) thing(x, y-1) else x
}

/*class Burmuda{
  @mutualrec final def one[A](x: A, y: Int): A = if(0 < y) two(x, y-1) else x
  @mutualrec final def two[A](x: A, y: Int): A = if(0 < y) one(x, y-1) else x
}*/

class TypeParamtersTest extends FlatSpec with Matchers{
  val fourK = 400000

  "A single argument, annotated method of a class with a type paramter" should "be equivalent to a tailrec" in{
    val fiji = new Fiji[Int]

    fiji.thing(1, fourK) should equal (1)
  }

  "A single argument, annotated method with a type parameter" should "be equivalent to a tailrec" in{
  	val largo = new KeyLargo

  	largo.thing(1, fourK) should equal (1)
  }

  "Two mutually recursive, single argument, annotated methods of a class with a type parameter" should "not throw a StackOverflow" in{
    val b = new Bahama[String]

    b.one("one", fourK) should equal ("one")
  }
}