package twotails

import org.scalatest.{ FlatSpec, Matchers }

class SpeedBump{
  @mutualrec final def one(x: Int, y: Int = 1): Int = if(0 < x) two(x-1) else y
  @mutualrec final def two(x: Int, y: Int = 2): Int = if(0 < x) one(x-1) else y
}

class Pothole{
  @mutualrec final def one(x: Int)(y: Int, z: Int = 1): Int = if(0 < x) two(x-1)(y) else z
  @mutualrec final def two(x: Int)(y: Int, z: Int = 2): Int = if(0 < x) one(x-1)(y) else z
}

class Ditch{
  @mutualrec final def one(x: Int, y: Int, z: Int = 1): Int = if(0 < x) two(x = x-1, y = y) else z
  @mutualrec final def two(x: Int, y: Int, z: Int = 2): Int = if(0 < x) one(y = y, x = x-1) else z
}

class GuardRail{
  @mutualrec final def one(v: Int, x: Int, y: Int = 1, z: Int = 1): Int = if(0 < v) two(v-1, x, z=z) else y
  @mutualrec final def two(v: Int, x: Int, y: Int = 2, z: Int = 2): Int = if(0 < v) one(v-1, x, z=z) else y
}

class DefaultArgumentTest extends FlatSpec with Matchers{
  val fourK = 400000

  "mutually recursive functions with default args" should "use the default args" in{
  	val sb = new SpeedBump
  	sb.one(fourK) should not equal sb.two(fourK)
  }

  "mutually recursive functions with default args and multi-param lists" should "use the default args" in{
  	val pt = new Pothole
  	pt.one(fourK)(fourK) should not equal pt.two(fourK)(fourK)
  }

  "mutually recursive function with default args called by name" should "use the default args" in{
  	val dt = new Ditch
  	dt.one(fourK, fourK) should not equal dt.two(fourK, fourK)
  }

  "mutually recursive function with default args with mixed calls" should "use the default args" in{
  	val gr = new GuardRail
  	gr.one(fourK, fourK) should not equal gr.two(fourK, fourK)
  }
}