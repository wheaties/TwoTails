# TwoTails

A compiler plugin to add a new phase to the compiler which supports mutual tail recursion.

[![Gitter](https://badges.gitter.im/wheaties/TwoTails.svg)](https://gitter.im/wheaties/TwoTails?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Using

Currently you'll have to check out this repo, compile and publish locally to take advantage of TwoTails. As it is a copiler plugin and has only been tested with 2.11 you'll have to add it to a project like so

```scala
libraryDependencies += compilerPlugin("com.github.wheaties" %% "twotails" % "0.0.1")
```

Then, within your code add the `mutualrec` annotation to methods which should be mutually tail recursive:

```scala
class Foo{
  @mutualrec final def one(x: Int, y: Int): Int = if(0 < x) two(x-1, y+1) else y
  @mutualrec final def two(x: Int, y: Int): Int = if(0 < x) one(y, x) else y
}
```

## Current Limitations

There are a number of caveats when working with the plugin at the moment

 * The types, arity, default values and even the variable names of the arguments have to match between mutually recursive calls.
 * The plugin does not currently support multiple parameter lists, i.e. `fn(x: Int)(y: Int): Int`.
 * The plugin does not support type parameters on the functions themselves, although type parameters are permitted on class or enclosing function definitions, i.e. `fn[A](x: A): A` is not supported.
 * As mentioned above, only has been tested and compiled against Scala 2.11

Stay tuned as this is a work in progress. That is, it's pre-alpha quality and should not go into production code!