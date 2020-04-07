---
title: Quick Guide to Scala Cats
description: todo
tags: scala, cats, fp, guide
tableOfContents: true
---

[Cats][web:cats] is popular library for functional programming in [Scala][web:scala] and in addition to great official documentation, there is a plethora of articles and blog posts describing this library in depth. I don't really want to repeat myself, so I decided to give this blog post different approach, more like a _cookbook_ style, with set of common problems and how to solve them using Cats. I also won't go in depth with explanations how used _type classes_ and _data types_ from Cats work, but instead I'll put links to official documentation, which definitely will do better job :)

<!-- MORE -->

# How to...

## ...compose two Future[Option[T]] values?
You can use a [monad transformer][web:monad-transformer] called [OptionT][web:cats/OptionT]. Let's say you have following code with two methods, both returning `Future[Option[T]]` and you want to compose them. Naively putting them into _for-comprehension_ won't work:

```scala
case class User(userId: Int)                                                      
case class Address(user: User)                                                    
                                                                                      
def getUserById(userId: Int): Future[Option[User]] = ???                          
def getAddressForUser(user: User): Future[Option[Address]] = ???                  
                                                                                      
val address = for {                                                               
  user    <- getUserById(123)                                                     
  address <- getAddressForUser(user)    // <- expects User, but gets Option[User] 
} yield address                                                                   
                                                                                      
// Error: type mismatch;                                                          
//   found   : Option[User]                                                       
//   required: User                                                               
//   address <- getAddressForUser(user) 
```

The _monad transformer_ `OptionT[F[_], A]` is basically just a wrapper for `F[Option[A]]`, in our case the `OptionT[Future, A]` for `Future[Option[A]]`. You can wrap the value into the transformer using the `OptionT()` apply method and unwrap it using the `.value` method, as shown below:

```scala
import cats.data.OptionT
import cats.implicits._

val address: OptionT[Future, Address] = for {
  user    <- OptionT(getUserById(123))         // Future[Option[User]]    -> OptionT[Future, User]
  address <- OptionT(getAddressForUser(user))  // Future[Option[Address]] -> OptionT[Future, Address]
} yield address
  
val result: Future[Option[Address]] = address.value   // unwrap value from OptionT
```

## ...have Either that can contain also both values?
While it may sound weird, there are cases when an [Either][scaladoc:Either] with not only `Left` and `Right`, but also `Both` value can be useful. Imagine that you need to validate username from some input form, one approach could be:

```scala
case class UserName(value: String) extends AnyVal
def validateUserName(userName: String): Either[String, UserName] =
  if (userName.isEmpty) Left("username cannot be empty")
  else                  Right(UserName(userName))
```

This is completely fine approach - either you get the user name, or error that the username is invalid. But imagine that there's new request from business that from now, usernames with dot inside are now deprecated. Now you have use case when you want to return both the deprecation error on left, but also the value on right, because this is not (yet) fatal error, it's more like warning. This is exactly what you can use the [Ior][web:cats/Ior] (_inclusive-or_) for. Let's demonstrate this in improved example from above:

```scala
def validateUserName(userName: String): Ior[String, UserName] =
  if      (userName.isEmpty)       Ior.left("username cannot be empty")
  else if (userName.contains(".")) Ior.both("username contains deprecated char", UserName(userName))
  else                             Ior.right(UserName(userName))
```

## ...have Either that collects errors?
todo

## ...have type-safe equality check?
Let's talk about comparing values in Scala. Normally the `==` and `!=` operators are used to compare two values, which desugars to Java's `.equals`, which looks like this:

```java
public boolean equals(Object obj);
```

See how the `equals` takes any object as its argument. This unfortunately leads to problem that we can actually compare two different types without getting compiler error:

```scala
"The answer" == 42 
// res0: Boolean = false
```

Fortunately we can use the [Eq][web:cats/Eq] _type class_ (and its handy symbolic operators `===` and `=!=`) from Cats which guarantees on type level that only two values of same type can be compared:

```scala
"The answer" === 42 

// cmd3.sc:1: type mismatch;
//  found   : Int(42)
//  required: String
// val res3 = "The answer" === 42
//                             ^
// Compilation Failed
```

## ...have type-safe non-empty list?
Imagine that you want to define new function `average` that takes list of integers and returns their average value. Naive approach could be this one:

```scala
def average(xs: List[Int]): Double = {
  xs.sum / xs.length.toDouble
}
```

Looks nice, nah? But wait, what if the input collection is empty?

```scala
average(List.empty)
// res0: NaN
```

Ouch, this is clearly something we need to handle. One solution is to define the return value as optional and return `None` if input is empty list:

```scala
def average(xs: List[Int]): Option[Double] =
  if (xs.isEmpty) None
  else            Some(xs.sum / xs.length.toDouble)
```

This works fine, but problem with the solution is that it actually masks the fact that the input is invalid, but it still accepts such input. Much better solution is to guarantee on type level that the input cannot be empty at all. This can be done using the [NonEmptyList][web:cats/NonEmptyList] data type, which is basically regular list, but won't allow to create empty collection:

```scala
import cats.data.NonEmptyList
import cats.syntax.list._

val one: NonEmptyList[Int]               = NonEmptyList.one(42)
val more: NonEmptyList[Int]              = NonEmptyList.of(1, 2, 3, 4)
val fromList1: Option[NonEmptyList[Int]] = NonEmptyList.fromList(List(1, 2, 3))
val fromList2: Option[NonEmptyList[Int]] = List(1, 2, 3).toNel
```

Now we can rewrite our example using this data type:

```scala
def average(xs: NonEmptyList[Int]): Double = {
  xs.reduceLeft(_ + _) / xs.length.toDouble
}
```

__Fun fact:__ because `NonEmptyList` doesn't allow empty values, unlike the regular `List`, it does have instance of [Semigroup][web:cats/Semigroup] _type class_, but doesn't (and cannot) have instance of [Monoid][web:cats/Monoid].

[web:cats]: https://typelevel.org/cats/
[web:cats/Eq]: https://typelevel.org/cats/typeclasses/eq.html
[web:cats/Ior]: https://typelevel.org/cats/datatypes/ior.html
[web:cats/Monoid]: https://typelevel.org/cats/typeclasses/monoid.html
[web:cats/NonEmptyList]: https://typelevel.org/cats/datatypes/nel.html
[web:cats/OptionT]: https://typelevel.org/cats/datatypes/optiont.html
[web:cats/Semigroup]: https://typelevel.org/cats/typeclasses/semigroup.html
[web:monad-transformer]: https://blog.buildo.io/monad-transformers-for-the-working-programmer-aa7e981190e7
[web:scala]: https://www.scala-lang.org/
[scaladoc:Either]: https://www.scala-lang.org/api/current/scala/util/Either.html