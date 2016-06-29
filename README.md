# TwoTails

A compiler plugin to add a new phase to the compiler which supports mutual tail recursion.

[![Gitter](https://badges.gitter.im/wheaties/TwoTails.svg)](https://gitter.im/wheaties/TwoTails?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Using

Within your code add the `mutualrec` annotation to methods which are mutually tail recursive:

```scala
import twotails.mutualrec

class Foo{
  @mutualrec final def one(x: Int, y: Int): Int = if(0 < x) two(x-1, y+1) else y
  @mutualrec final def two(x: Int, y: Int): Int = if(0 < x) one(y, x) else y
}
```

Mutual tail recursion may only be added to methods and not class constructors, vals or vars. If a single method is annotated it will be replaced by `@tailrec`. Similarly to `@tailrec`, methods annotated must be effectively final and the return type explicitly provided (effectively final being defined as `final`, `private`, part of a package object, or within a `def`.) Methods not within the same parent scope, for example two methods on two different distinct classes, will not be optimized and will result in a compilation error.

## Including

Twotails is currently published to Sonatype and the latest version is 0.2.0. To include this plugin for your project add the following two lines to your build file:

```scala
libraryDependencies ++= Seq(
  compilerPlugin("com.github.wheaties" %% "twotails" % "0.2.0"),
  "com.github.wheaties" %% "twotails-annotations" % "0.2.0"
)
```

TwoTails has been cross compiled against Scala 2.11 and 2.12.0-M5 with the intention of supporting 2.12 proper in the future. If you'd like 2.10 support, open a PR. Help is always appreciated.

## Current Limitations

There are two known issues that reduce the effectiveness of this plugin:

 * It currently handles only "simple" single-branching recursive structures. That is, it can only handle functions which do not have multiple function calls. The following code will fail to compile:

```scala
import twotails.mutualrec

class Foo{
  @mutualrec def one(x: Int): Int = if(0 < x) two(x-1) else x
  @mutualrec def two(x: Int): Int = if(0 < x) one(x-1) else three(x)
  @mutualrec def three(x: Int): Int = if(0 < x) one(x-1) else x
}
```

 * It does not handle method [size limits](http://stackoverflow.com/questions/17422480/maximum-size-of-a-method-in-java-7-and-8).

All of these aspects are active areas of exploration.
