package twotails

class Foo{
  @mutualrec def yo(x: Int): Int = if(0 < x) yo(x-1) else 0
}

//TODO: add in scalatest?
object MutualRecTest{
  val a = new Foo

  Console.println(a.yo(70000))
}