package twotails

import org.scalatest.{ FlatSpec, Matchers }

class Hive{
  @mutualrec final def one(x: Int, y: => Int): Int = if(0 < x) two(x-1, 1) else 0
  @mutualrec final def two(x: Int, y: => Int): Int = if(0 < x) one(x-1, 1) else 0
}

class YelloJacket{
  @mutualrec final def one(x: Int, y: => Int): Int = if(0 < x) two(x-1, y) else y
  @mutualrec final def two(x: Int, y: => Int): Int = if(0 < x) one(x-1, y) else y
}

class Wasp{
  @mutualrec final def one(x: Int, y: => Int): Int = if(0 < x) two(x-y, 1) else y
  @mutualrec final def two(x: Int, y: => Int): Int = if(0 < x) one(x-y, 1) else y
}

//TODO: This is really just a test of the conditions upon which "mkDone" works.
class Stinger{
  private def that(f: Int): () => Int = { () => f }

  @mutualrec final def one(x: Int, y: => Int): () => Int = 
    if(0 < x) two(x-y, 1) else that(1)
  @mutualrec final def two(x: Int, y: => Int): ()=> Int = 
    if(0 < x) one(x-y, 1) else that(2)
}

//TODO: This really is more of a "does this compile" thing. Move to Partest.
/*class Hornet{
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

//TODO: This really is more of a "does this compile" thing. Move to Partest.
class QueenBee{
  @mutualrec final def one(x: Int, y: => Either[Int,Int]): Either[Int,Int] = if(0 < x) two(x-1, y) else y
  @mutualrec final def two(x: Int, y: => Either[Int,Int]): Either[Int,Int] = if(0 < x) one(x-1, y) else y
}

//TODO: This really is more of a "does this compile" thing. Move to Partest.
class Larva{
  @mutualrec final def one(x: Int)(y: => Int): Int = if(0 < x) two(x-1)(y) else y
  @mutualrec final def two(x: Int)(y: => Int): Int = if(0 < x) one(x-1)(y) else y
}

//TODO: Partest: this tests the ownership of "z" when transforming from named to Function0.
class Drone{
  @mutualrec final def one(x: Int)(y: => Int): Int = if(0 < x) two(x-1){
    val z = (x % 8096) - 1
    z+1
  } else y
  @mutualrec final def two(x: Int)(y: => Int): Int = if(0 < x) one(x-1)(y) else y
}

//TODO: This really is more of a "does this compile" thing. Move to Partest.
class Honey[T <: Int]{
  @mutualrec final def one(x: Int, y: => T): T = if(0 < x) two(x-1, y) else y
  @mutualrec final def two(x: Int, y: => T): T = if(0 < x) one(x-1, y) else y
}*/

class ByNameTest extends FlatSpec with Matchers{
  val fourK = 400000

  "mutually recursive functions with byname parameters returning constant literals" should "work" in{
    val hive = new Hive

    hive.one(fourK, 1 + 2) should equal(0)
  }

  "mutually recursive functions with byname parameters" should "just work" in{
    val yellow = new YelloJacket

    yellow.one(fourK, 5) should equal(5)
  }

  "mutually recursive functions with byname parameters #2" should "just work" in{
  	val wasp = new Wasp

  	noException should be thrownBy wasp.one(fourK, 5)
  	wasp.one(fourK, fourK) should equal(1)
  }
}