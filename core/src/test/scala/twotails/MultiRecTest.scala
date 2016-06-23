package twotails

import org.scalatest.{ FlatSpec, Matchers }

class Cup{
  @mutualrec final def one(x: Int, y: Int = 0): Int = if(0 < x) two(x-1, y) else y
  @mutualrec final def two(u: Int, v: Int = 0): Int = if(0 < u) one(u-1, v) else v

  @mutualrec final def three(x: Int, y: Int = 1): Int = if(0 < x) four(x-1, y) else y
  @mutualrec final def four(u: Int, v: Int = 1): Int = if(0 < u) three(u-1, v) else v
}

class Bowl{
  @mutualrec final def one[A](x: Int, y: A): A = if(0 < x) two(x-1, y) else y
  @mutualrec final def two[A](u: Int, v: A): A = if(0 < u) one(u-1, v) else v

  @mutualrec final def three(x: Int, y: Int = 1): Int = if(0 < x) four(x-1, y) else y
  @mutualrec final def four(u: Int, v: Int = 1): Int = if(0 < u) three(u-1, v) else v
}

class MultiRedTest extends FlatSpec with Matchers{
  val fourK = 400000

  "a class with two sets of mutually recursive functions" should "just work" in{
  	val cup = new Cup
  	cup.one(fourK) should equal(0)
  	cup.three(fourK) should equal(1)
  }

  "a class with two sets of mutually recursive functions but different types" should "just work" in{
  	val bowl = new Bowl
  	bowl.one(fourK, "a") should equal("a")
  	bowl.three(fourK) should equal(1)
  }
}