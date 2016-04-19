package twotails

class Foo{
  @mutualrec def yo(x: Int): Int = if(0 < x) yo(x-1) else 0
}

class Bar{
  @mutualrec def one(x: Int): Int = if(x < 0) two(x-1) else x
  @mutualrec def two(x: Int): Int = if(x < 0) one(x-2) else x
}

//TODO: add in scalatest?
object MutualRecTest{
  val a = new Foo

  Console.println(a.yo(70000))
}