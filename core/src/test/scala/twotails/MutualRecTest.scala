package twotails

class Foo{
  @mutualrec def yo = 2
}

object MutualRecTest{
  val a = new Foo

  Console.println(a.yo)
}