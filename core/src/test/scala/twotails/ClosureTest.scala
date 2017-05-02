package twotails

import org.scalatest.{ FlatSpec, Matchers }

final class Pony{
  @mutualrec def one(x: Int, f: () => Int): Int = if(x < 0) f() else two(x-1, {() => x + f()})
  @mutualrec def two(x: Int, f: () => Int): Int = if(x < 0) f() else one(x-1, {() => x + f()})
}

//The point of this class is to force the capture of the "x" variable within the scope of each
//of "one" and "two."
final class Horse{
  @mutualrec def one(x: Int)(f: () => Int): Int = if(f != null){
  	if(x < 0) f() else two(x-1)(f)
  } 
  else one(x){() => x}

  @mutualrec def two(x: Int)(f: () => Int): Int = if(f != null){
  	if(x < 0) f() else one(x-1)(f)
  }
  else two(x){() => x}
}

class ClosureTest extends FlatSpec with Matchers{
  val fourK = 400000

  "closures" should "work as expected" in{
  	val pony = new Pony

  	//this'll throw a StackOverflow cause it'll recursively call itself if the plugin has a bug.
  	noException should be thrownBy pony.one(0, {() => 0})
  	pony.one(10, {() => 0}) should equal(10+9+8+7+6+5+4+3+2+1)
  }

  "closed over state" should "not convert immutable to mutable" in{
  	val horse = new Horse

  	horse.one(fourK)(null) should equal(fourK)
  }
}