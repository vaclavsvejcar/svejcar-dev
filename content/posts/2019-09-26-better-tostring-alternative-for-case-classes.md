---
title: Better 'toString' alternative for case classes
description: Default toString method of Scala case class doesn't show field names. But there's alternative how to implement such feature using the Cats and Kittens library.
tags: scala, cats, shapeless
tableOfContents: true
---

One of the great advantages of [Scala]'s [case classes][case class] is that compared to regular classes, bunch of useful methods, such as `equals`, `hashCode` and `toString`, are automatically generated. The generated `toString` method is nice, because it includes actual field values of the displayed case class.

```scala
case class Test(name: String, value: Int) 
val test = Test("The Answer", 42)

test.toString   // Test(The Answer,42)
```

The above output is good, but for case classes with many fields, nested structures or just lots of similar fields, it can become not so clear which value belongs to which field, as shown below:

```scala
case class Config(poolSize: Int, maxConnections: Int, batchSize: Int, intervalLength: Int, maxTimeout: Int) 
val config = Config(45, 23, 10, 10, 5000)

config.toString // Config(45,23,10,10,5000)
```

Wouldn't it help if we could also see field names for these values? In this article, I'll show how this can be done without much boilerplate using [Cats] and [Kittens] libraries.

<!-- MORE -->

# Before we begin
All code examples below assume that you have [Cats] and [Kittens] libraries imported into your project. You can do that by adding following lines to `build.sbt`:

```scala
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "kittens" % "2.0.0"
)
```

This article also assumes that you're familiar with concept of [typeclasses] and you have and least basic knowledge of the [Cats] library.

# Using Show typeclass
Before deciding how to eventually implement the `toString` method to match our requirements, we should discuss if `toString` is really the way we want to use. Implementing it for our case classes is fine, but what if we want to define it for case class from 3rd party libraries? Extending such case class, even if possible, is obviously not the most nice way.

So instead of overriding the `toString` method, we will use the [Show typeclass] from [Cats] library. It allows us to implement instance of `Show` typeclass for any case class, either ours or from 3rd party library. Let's see how we can implement very dummy version for previously shown `Config` case class, that will also print field names.

```scala
import cats._
import cats.implicits._

case class Config(poolSize: Int, maxConnections: Int, batchSize: Int, intervalLength: Int, maxTimeout: Int)
object Config {
  implicit val showConfig: Show[Config] = Show { instance =>
    s"Config(poolSize=${instance.poolSize}, maxConnections=${instance.maxConnections}, " +
      s"batchSize=${instance.batchSize}, intervalLength=${instance.intervalLength}, maxTimeout=${instance.maxTimeout})"
  }
}

val config = Config(...)
config.show // Config(poolSize=45, maxConnections=23, batchSize=10, intervalLength=10, maxTimeout=5000)
```

This results in much better looking output, but the implementation is really cumbersome and definitely something we don't want to do in real-world codebase. But there's way how to generate all this stuff automatically.

# Automatic derivation of Show typeclass
Here comes the [Kittens] library to the rescue. It offers automatic derivation of typeclasses from [Cats] using [Shapeless], meaning it can also automatically [derive instance of Show typeclass][derive show] for our case classes. One of the advantages of automatically derived `Show` instance is that it also renders case class field names:

```scala
import cats._
import cats.implicits._

case class Config(poolSize: Int, maxConnections: Int, batchSize: Int, intervalLength: Int, maxTimeout: Int)
object Config { implicit val showConfig: Show[Config] = derived.semi.show }

val config = Config(...)
config.show // Config(poolSize = 45, maxConnections = 23, batchSize = 10, intervalLength = 10, maxTimeout = 5000)
```

This is exactly what we wanted to achieve, and this time without all that hand-written boilerplate code. In [Kittens], this method of automatic derivation is called _semi-auto_, as we still need to define the implicit value of `Show` instance on our own. There is also option to use _full-auto_ derivation:

```scala
import cats._
import cats.implicits._

case class Config(poolSize: Int, maxConnections: Int, batchSize: Int, intervalLength: Int, maxTimeout: Int)

import derived.auto.show._  // enables fully automatic derivation

val config = Config(...)
config.show // Config(poolSize = 45, maxConnections = 23, batchSize = 10, intervalLength = 10, maxTimeout = 5000)
```
When using the _full-auto_ mode, we don't even need to define the `Show` instance as _implicit value_, so there's less boilerplate. However, we lose some control over where and what is automatically derived. For that reason, I'd personally recommend using the _semi-auto_ derivation instead. You can also check [Kittens] documentation for all available [derivation modes].

# Changes in Scala 2.13
The above solution is based on the fact that in [Scala 2.12] and older, you can easily get field values of case class using the [productIterator] method (since each _case class_ inherits from the `Product` trait), but there's no nice way how to get field names. However, in [Scala 2.13], there's new method [productElementNames], that returns collection of field names. Using this new method, we can write really simple implementation of `toString` alternative, that will return same result as the above solution using [Kittens]:

```scala
def show(product: Product): String = {
  val className = product.productPrefix
  val fieldNames = product.productElementNames.toList
  val fieldValues = product.productIterator.toList
  val fields = fieldNames.zip(fieldValues).map { case (name, value) => s"$name = $value"}
  
  fields.mkString(s"$className(", ", ", ")")
}

case class Test(label: String, value: Int)
val test = Test("The Answer", 42)

show(test)   // Test(label = The Answer, value = 42)
```

This solution might be preferred if you dont need compatibility with older _Scala_ versions, as it doesn't need any external libraries to be used.

[Cats]: https://typelevel.org/cats/
[case class]: https://docs.scala-lang.org/tour/case-classes.html
[derive show]: https://github.com/typelevel/kittens#derive-show
[derivation modes]: https://github.com/typelevel/kittens#three-modes-of-derivation
[Kittens]: https://github.com/typelevel/kittens
[productElementNames]: https://www.scala-lang.org/api/2.13.0/scala/Product.html#productElementNames:Iterator[String]
[productIterator]: https://www.scala-lang.org/api/2.12.8/scala/Product.html#productIterator:Iterator[Any]
[Scala]: https://www.scala-lang.org/
[Scala 2.12]: https://www.scala-lang.org/news/2.12.0/
[Scala 2.13]: https://www.scala-lang.org/news/2.13.0/
[Shapeless]: https://github.com/milessabin/shapeless
[Show typeclass]: https://typelevel.org/cats/typeclasses/show.html
[typeclasses]: https://scalac.io/typeclasses-in-scala/