---
title: Quick Guide to Scala Cats
description: todo
tags: scala, cats, fp, guide
tableOfContents: true
---

[Cats][web:cats] is popular library for functional programming in [Scala][web:scala] and in addition to great official documentation, there is a plethora of articles and blog posts describing this library in depth. I don't really want to repeat myself, so I decided to give this blog post different approach, more like a _cookbook_ style, with set of common problems and how to solve them using Cats. I also won't go in depth with explanations how used _type classes_ and _data types_ from Cats work, but instead I'll put links to official documentation, which definitely will do better job :)

<!-- MORE -->

# How to...

## ...correctly use imports from Cats?
In short, the easiest way is just to import everything that's commonly needed:

```scala
import cats._
import cats.data._
import cats.implicits._
```

But if you want to know more about how Cats' imports are organized, don't forget to check [Cats FAQ][web:cats/faq#imports] and [Imports Guide][web:cats/imports]. Basically, Cats' imports are organized into few basic packages:

- __cats.___ - this package contains common [type classes][web:cats/typeclasses], such as [Applicative][web:cats/Applicative], [Monoid][web:cats/Monoid] or [Semigroup][web:cats/Semigroup]. You can import the entire package (`import cats._`), or use individual imports, such as `import cats.Semigroup`.
- __cats.data.___ - this one contains [data types][web:cats/datatypes], such as [Ior][web:cats/Ior], [Validated][web:cats/Validated] or [NonEmptyList][web:cats/NonEmptyList].
- __cats.instances.x.___ - contains instances of _type classes_ for specified type, such as `import cats.instances.option._`.
- __cats.syntax.x.___ - contains extension methods for selected data type, so you can use for example `12.some` instead of `Some(12)`, if you put the `import cats.syntax.option._` in your code.

And then there is the __cats.implicits.___, which basically imports all _type classes_, instances and syntax you need.

So should you import just the stuff you need instead of importing everything? Well, while it might improve things such as compilation time, I don't really recommend that unless you really know what you're doing, because __mixing incorrect imports__ together will __lead to pretty confusing compiler errors__, like this one:

```scala
import cats.implicits._
import cats.instances.option._

42.pure[Option]
//Error: Could not find an instance of Applicative for Option
//  42.pure[Option]
```

Guess what's wrong here? Actually both `cats.implicits` and `cats.instances.option` both extends from `cats.instances.OptionInstances`, which confuses compiler. To make this work, you'd have to import syntax for _Applicative_ (`cats.syntax.applicative._`) - that's the `.pure` method, and also instance of _Applicative_ for `Option` - `cats.instances.option._`:

```scala
import cats.instances.option._
import cats.syntax.applicative._

42.pure[Option]
```

As you already guess, this might become trickier on larger project, so my recommendation - always use that three imports from the beginning to keep both compiler and your co-workers happy.

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

## ...have Either that accumulates errors?
Imagine that you have form validation defined like this:

```scala
sealed trait FormError
case object UsernameHasSpecialChars     extends FormError
case object PasswordDoesNotMeetCriteria extends FormError

case class ValidForm(userName: String, password: String)

def validateUserName(userName: String): Either[FormError, String] =
  Either.cond(
    userName.matches("^[a-zA-Z0-9]+$"),
    userName,
    UsernameHasSpecialChars
  )
def validatePassword(password: String): Either[FormError, String] =
  Either.cond(
    password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$"),
    password,
    PasswordDoesNotMeetCriteria
  )

def validateForm(rawUserName: String, rawPassword: String): Either[FormError, ValidForm] =
  for {
    userName <- validateUserName(rawUserName)
    password <- validatePassword(rawPassword)
  } yield ValidForm(userName, password)
```

It's done in nice functional way, where the function for validating entire form is composed from functions for individual field validations, but it has one major problem - it cannot collect the errors on the `Left` side of `Either`. You'll always get only the very first error and it's really not really user friendly to ask user to corrent incorrect values in form field by field:

```scala
validateForm("invalidUsername@#$@$~", "invalidPassword")
// res0: Left(UsernameHasSpecialChars)
```

So what's going on here? Main problem is the `for-comprehension`, which is just syntactic sugar for `flatMap` calls. Desugared, it would look like this:

```scala
def validateForm(rawUserName: String, rawPassword: String): Either[FormError, ValidForm] =
  validateUserName(rawUserName)
    .flatMap(userName =>
      validatePassword(rawPassword)
        .map(password => ValidForm(userName, password))
    )

```

And `flatMap` itself is method of the [Monad][web:cats/Monad] _type class_. Problem with _Monad_ and `flatMap` is that it represents sequential (dependent) operation, where current computation can be done only if the result of previous one is available:

```scala
def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
```

In case of the form validation demo, if the first validation returns `Left` with error, the _for-comprehension_ short-circuits, because there's no `Right` value to continue with.

Instead, we want to use the [Validated][web:cats/Validated] data type and compose them using the [Applicative][web:cats/Applicative], which can represent independent operations (and that validation of individual form field is):

```scala
import cats.data.ValidatedNec
import cats.implicits._

type ValidationResult[A] = ValidatedNec[FormError, A]

def validateUserName(userName: String): ValidationResult[String] =
  if (userName.matches("^[a-zA-Z0-9]+$")) userName.validNec
  else UsernameHasSpecialChars.invalidNec

def validatePassword(password: String): ValidationResult[String] =
  if (password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$")) password.validNec
  else PasswordDoesNotMeetCriteria.invalidNec

def validateForm(userName: String, password: String): ValidationResult[ValidForm] =
  (validateUserName(userName), validatePassword(password)).mapN(ValidForm)
```

Now we can collect all validation errors as shown below:

```scala
validateForm("invalidUsername@#$@$~", "invalidPassword")
// res0: Invalid(Chain(UsernameHasSpecialChars, PasswordDoesNotMeetCriteria))
```

Check the official documentation for more details, and also don't forget to check the [NonEmptyChain][web:cats/NonEmptyChain] data type, which is used here to represent non-empty collection of errors (`ValidatedNel[A]` is just type alias for `Validated[NonEmptyChain[DomainValidation], A]`).

## ...have type-safe equality check?
Let's talk about comparing values in Scala. Normally the `==` and `!=` operators are used to compare two values, which desugars to Java's `.equals`, which looks like this:

```java
public boolean equals(Object obj);
```

See how the `equals` takes any object as its argument? This unfortunately leads to problem that we can actually compare two different types without getting compiler error:

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

## ...use Cats' extension methods?
This is one of the most basic features, but _Cats_ provides extension methods for most used _data types_ for constructing their values.

__Option__
```scala
import cats.implicits._

42.some
// res0: Option[Int] = Some(42)

none[Int]
// res1: Option[Int] = None
```

__Either__
```scala
import cats.implicits._

"The Answer".asRight[Int]
// res0: Either[Int, String] = Right("The Answer")

42.asLeft[String]
// res1: Either[Int, String] = Left(42)
```

[web:cats]: https://typelevel.org/cats/
[web:cats/datatypes]: https://typelevel.org/cats/datatypes.html
[web:cats/faq#imports]: https://typelevel.org/cats/faq.html#what-imports
[web:cats/imports]: https://typelevel.org/cats/typeclasses/imports.html
[web:cats/typeclasses]: https://typelevel.org/cats/typeclasses.html
[web:cats/Applicative]: https://typelevel.org/cats/typeclasses/applicative.html
[web:cats/Eq]: https://typelevel.org/cats/typeclasses/eq.html
[web:cats/Ior]: https://typelevel.org/cats/datatypes/ior.html
[web:cats/Monad]: https://typelevel.org/cats/typeclasses/monad.html
[web:cats/Monoid]: https://typelevel.org/cats/typeclasses/monoid.html
[web:cats/NonEmptyList]: https://typelevel.org/cats/datatypes/nel.html
[web:cats/NonEmptyChain]:https://typelevel.org/cats/datatypes/chain.html#nonemptychain
[web:cats/OptionT]: https://typelevel.org/cats/datatypes/optiont.html
[web:cats/Semigroup]: https://typelevel.org/cats/typeclasses/semigroup.html
[web:cats/Validated]: https://typelevel.org/cats/datatypes/validated.html
[web:monad-transformer]: https://blog.buildo.io/monad-transformers-for-the-working-programmer-aa7e981190e7
[web:scala]: https://www.scala-lang.org/
[scaladoc:Either]: https://www.scala-lang.org/api/current/scala/util/Either.html