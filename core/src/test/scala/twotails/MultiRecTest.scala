package twotails

import org.scalatest.{ FlatSpec, Matchers }
import java.lang.StackOverflowError

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

class Plate{
  @mutualrec final def zero(x: Int): Int = if(x < 0) x else one(x-1)
  @mutualrec final def one(x: Int): Int = if(x < 0) two(x, x) else zero(x-1)

  @mutualrec final def two(x: Int, y: Int): Int = if(x < 0) y else three(x-1, y+1)
  @mutualrec final def three(x: Int, y: Int): Int = if(x < 0) one(x) else two(x-1, y+1)
}

class Saucer{
  @mutualrec final def zero(x: Int): Int = if(x < 0) x else one(x-1)
  @mutualrec final def one(x: Int): Int = if(x < 0) zero(x-1) else two(x, x)

  @mutualrec final def two(x: Int, y: Int): Int = if(x < 0) y else three(x-1, y+1)
  @mutualrec final def three(x: Int, y: Int): Int = if(x < 0) two(x-1, y+1) else one(x)
}

class MultiRecTest extends FlatSpec with Matchers{
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

  "a class with recursive functions of different types" should "not be transformed together" in{
    val saucer = new Saucer

    intercept[StackOverflowError]{
      saucer.three(fourK, fourK)
    }
  }
}