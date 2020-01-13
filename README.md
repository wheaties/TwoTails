# TwoTails

A compiler plugin to add a new phase to the compiler which supports mutual tail recursion.


[![Typelevel incubator](https://img.shields.io/badge/typelevel-incubator-F51C2B.svg)](http://typelevel.org) [![Build Status](https://travis-ci.org/wheaties/TwoTails.svg?branch=master)](https://travis-ci.org/wheaties/TwoTails) [![Gitter](https://badges.gitter.im/wheaties/TwoTails.svg)](https://gitter.im/wheaties/TwoTails?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

## Using

Within your code add the `mutualrec` annotation to methods which are mutually tail recursive:

```scala
import twotails.mutualrec

class Foo{
  @mutualrec final def one(x: Int, y: Int): Int = if(0 < x) two(x-1, y+1) else y
  @mutualrec final def two(x: Int, y: Int): Int = if(0 < x) one(y, x) else y
}
```

Some rules to follow to get the code to compile when working with this plugin:

1. Mutual tail recursion may only be added to methods which are not class constructors; this naturally precludes `val`, `lazy val`, or `var`. 
2. If a single method is annotated with `mutualrec` it will be replaced by a `@tailrec` annotation.
3. Similarly to `@tailrec`, methods annotated must be effectively final and the return type explicitly provided (effectively final being defined as `final`, `private`, part of a package object, or within a `def`.) 
4. Methods not within the same parent scope, for example two methods on two different distinct classes, will not be optimized and will result in a compilation error.
5. Methods with different type-signatures will not be optimized and will result in a compilation error.

## Including

Twotails is currently published to Sonatype and the latest version is 0.3.1. To include this plugin for your project add the following two lines to your build file:

```scala
libraryDependencies ++= Seq(
  compilerPlugin("com.github.wheaties" %% "twotails" % "0.3.1" cross CrossVersion.full),
  "com.github.wheaties" %% "twotails-annotations" % "0.3.1" cross CrossVersion.full
)
```

TwoTails has been cross compiled and published against Scala 2.11.8. Tests are run against 2.11.7, 2.11.8 and 2.12.0 with the intention of supporting 2.12.x and Scala.js in the future. If you'd like 2.10 support, open a PR. Help is always appreciated. No guarantee exists

## Current Limitations

TwoTails does not handle method [size limits](http://stackoverflow.com/questions/17422480/maximum-size-of-a-method-in-java-7-and-8). This leads to two problems:

1. For a large groups of functions or for a combination of functions where one method relies heavily on complex pattern matching (which by itself can cause this issue), the plugin can cause the code to fail to compile. 
2. Large methods can prevent the JIT from making certain types and forms of optimizations. It is suggested that if performance is an issue, benchmark the code.

TwoTails adds in a code transforming phase to the compiler. As such, code using macros or code enhancing techniques (such as that in [Spring](https://spring.io/)) may fail to compile at best or work in an unexpected manner at worst. We want to specifically call out [AoP](https://en.wikipedia.org/wiki/Aspect-oriented_programming) libraries such as [AspectJ](https://eclipse.org/aspectj/) where the intention of an author is to have each "loop" of the recursive call trigger some  side-effect. In such cases, similar to code using `@tailcall`, only the first "loop" will induce the effect; all others will have been compiled away.

## Participation

PRs are welcome and encouraged. Please do not leave any defamatory remarks or swear words in the comments of code submitted. TODOs may be left in so long as they are informative. Unit tests are nice but compilation tests nicer (a `class` or `object` stressing the plugin.) Purposefully failing constructs may be left in one of the unit test sections commented out until the project can be set up to run with [Scala-Partest](https://github.com/scala/scala-partest). 

Bug reports are appreciated. Bug reports submitted with reproducible errors are more appreciated. Reports of issues with other compiler plugins should be submitted to all plugin projects. New features or feature requests are not bugs.

TwoTails supports the Typelevel [Code of Conduct](http://typelevel.org/conduct.html). All conversations within the Gitter room and/or other public online venues are asked to respect and adhere to the terms laid out therein (tl;dr? don't be rude, belittle or harass others.) Mutual recursion demands mutual respect.
