package twotails

import org.scalatest.{ FlatSpec, Matchers }

final class Pony{
  @mutualrec def one(x: Int, f: () => Int): Int = if(x < 0) f() else two(x-1, {() => x + f()})
  @mutualrec def two(x: Int, f: () => Int): Int = if(x < 0) f() else one(x-1, {() => x + f()})
}

final class Horse{
  @mutualrec def one(x: Int)(f: () => Int = null): Int = if(f == null) one(x){() => x} else{
  	if(x < 0) f() else two(x-1)(f)
  }
  @mutualrec def two(x: Int)(f: () => Int = null): Int = if(f == null) two(x){() => x} else{
  	if(x < 0) f() else one(x-1)(f)
  }
}

class ClosureTest extends FlatSpec with Matchers{
  val fourK = 400000

  "closures" should "work as expected" in{
  	val pony = new Pony

  	pony.one(10, {() => 0}) should equal(10+9+8+7+6+5+4+3+2+1)
  }

  "closed over state" should "not convert immutable to mutable" in{
  	val horse = new Horse

  	horse.one(fourK)() should equal(fourK)
  }
}