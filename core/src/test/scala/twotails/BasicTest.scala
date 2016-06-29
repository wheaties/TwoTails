package twotails

import org.scalatest.{ FlatSpec, Matchers }
import annotation.{tailrec, switch}
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

class Rabbit{
  @mutualrec final def one(count: Int): Either[String,Int] = 
    if (count < 0) Left("0") else two(count)
  @mutualrec final def two(count: Int): Either[String,Int] = 
    if (count < 0) Left("0") else if (count == 0) Right(0) else three(count)
  @mutualrec final def three(count: Int): Either[String,Int] = if (count == 0) Right(0) else one(count-1)
}

/*class Dog{
  @mutualrec final def dog1(count: Int): Int = if (count < 0) 0 else dog2(count - 1)
  @mutualrec final def dog2(count: Int): Int = if (count < 0) dog1(count) else dog3(count)
  @mutualrec final def dog3(count: Int): Int = if (count < 0) 0 else dog1(count - 1)
}*/

/*class Husky{
  @mutualrec final def dog1(count: Int): Int = if (count == 0) count else dog2(count - 1)
  @mutualrec final def dog2(count: Int): Int = try{
  	if (count == 0) count else dog1(count - 1)
  }
  catch{
  	case _ => 1
  }
}*/

/*class Moose{
  @mutualrec final def one(count: Int): Int = (count: @switch) match{
  	case 0 => 0
  	case 1 => three(0)
  	case _ => two(count-1)
  }
  @mutualrec final def two(count: Int): Int = (count: @switch) match{
  	case 0 => 0
  	case 1 => one(0)
  	case _ => three(count-1)
  }
  @mutualrec final def dog3(count: Int): Int = (count: @switch) match{
  	case 0 => 0
  	case 1 => two(0)
  	case _ => one(count-1)
  }
}*/

final class Chipmunk{
  @mutualrec def one(x: Int): Int = if(x < 0) throw new Exception("boom!") else two(x-1)
  @mutualrec def two(x: Int): Int = if(x < 0){
    throw new Exception("bam!")
  } 
  else one(x-1)
}

class BasicTest extends FlatSpec with Matchers{
  val fourK = 400000
  
  "A single argument, annotated method" should "be equivalent to a tailrec" in{
    val foo = new Foo
    val foo2 = new Foo2

    foo.yo(fourK) should equal {
      foo2.yo(fourK)
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

  "An exception thrown by a mutually recursive function" should "have the correct position" in{
    val chip = new Chipmunk

    val ex = intercept[Exception]{
      chip.one(fourK)
    }
    val ex2 = intercept[Exception]{
      chip.two(fourK)
    }
    
    ex.getStackTrace()(0).getLineNumber() should equal(70)
    ex2.getStackTrace()(0).getLineNumber() should equal(68)
  }
}