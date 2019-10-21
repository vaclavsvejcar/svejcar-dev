---
title: Extracting case class field names with Shapeless
tags: scala, shapeless
withTOC: true
---

In my previous job, we were working on backend system that regularly fetched data from other services, both 3rd party and inhouse. These data were parsed and mapped into our internal data structure modelled with _case classes_. The fetching itself was usually done using some _REST API_ and was mostly straightforward, except for one service, where we had to explicitly list all fields of given entity for which we wanted to fetch new data. This was done using _REST_ call similar to this:

```txt
GET /entities/User?fields=id,name,surname
```

So based on the fact that this entity would be mapped in our backend as _case class_ `User`, how can we get all its field names to build the above query? In this blog post, I'd like to share the simple solution, based on some _generic derivation_ using [Shapeless][shapeless].

<!-- MORE -->

# Before we start
This blog post assumes at least basic knowledge of what [Shapeless][shapeless] is and what is the [HList][shapeless_HList] and the [Aux pattern]. Here is the list of nice resources to start with:

- [The Type Astronaut's Guide to Shapeless] - amazing free book that serves as intro into _Shapeless_, can't recommend it more
- [Bits of Shapeless part 1: HLists] - nice intro into _HLists_
- [Aux pattern] - introduction into _Aux pattern_, vastly used by _Shapeless_ to overcome some limitations of _Scala's_ type system

# Naive implementation
Let's start with the definition of our example case class:

```scala
case class User(id: Long, name: String, surname: String)
```

So the goal is clear, we need to write piece of code that extracts field names from any given _case class_, without the need to reimplement it again and again for every _case class_ in our domain. So instead of working with concrete _types_ (let's say the `User` _case class_), we want to work with the _shape_, more _generic_ representation, something like _list of fields_.

## Attempt with Generic
Let's try to find way how to convert any _case class_ into its _generic_ representation. _Shapeless_ offers [Generic][shapeless_Generic] class, that does this for us:

```scala
import shapeless._

val genUser = Generic[User]
// genUser: Generic[User]{type Repr = Long :: String :: String :: shapeless.HNil}
```

As you can see, the `Repr` type represents the generic representation of our _case class_, encoded as [HList][shapeless_HList]. But problem with this representation is that it only contains field types, not the names. To solve this, we need to use different class, the [LabelledGeneric][shapeless_LabelledGeneric].

## LabelledGeneric to the rescue
[LabelledGeneric][shapeless_LabelledGeneric] is similar to [Generic][shapeless_Generic], but it also encodes field names:

```scala
import shapeless._

val genUser = LabelledGeneric[User] 
genUser: LabelledGeneric[User]{type Repr = Long with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("id")],Long] :: String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("name")],String] :: String with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[String("surname")],String] :: shapeless.HNil}
```

Ok, the `Repr` type now looks pretty complex, but you can see the pattern there: each field is represented by its type with `KeyTag`, which contains the field name. The question is, how to extract these field names to something like `List("id", "name", "surname")`? This is exactly what the [Keys][shapeless_Keys] class is for.

## Extracting field names
```scala
import shapeless._
import shapeless.ops.record._

val genUser = LabelledGeneric[User] 
val keys = Keys[genUser.Repr].apply 
// keys: Symbol with tag.Tagged[id] :: Symbol with tag.Tagged[name] :: Symbol with tag.Tagged[surname] :: HNil = 'id :: 'name :: 'surname :: HNil
val keyList = keys.toList.map(_.name)
// keyList: List[String] = List("id", "name", "surname")
```

_Et voilà_. We got the list of field names, exactly as needed. But problem is that this code is bound to the `User` _case class_. Let's do the last thing and make the code more _generic_.

# Making things more generic
Here is the code for final, generic solution:
```scala
trait Attributes[T] {
  def fieldNames: List[String]
}

object Attributes {
  implicit def toNames[T, Repr <: HList, KeysRepr <: HList](
    implicit gen: LabelledGeneric.Aux[T, Repr],
    keys: Keys.Aux[Repr, KeysRepr],
    traversable: ToTraversable.Aux[KeysRepr, List, Symbol]
  ): Attributes[T] = new Attributes[T] {
    override def fieldNames: List[String] = keys().toList.map(_.name)
  }

  def apply[T](implicit attributes: Attributes[T]): Attributes[T] = attributes
}
```

As you can see, it's pretty similar to previous code, with the difference that instances of `Keys`, `LabelledGeneric` etc are now resolved from _implicit values_. These are basically provided by shapeless, backed up by some heavy _macro_ machinery, but we really don't need to care about this detail. Rest of the code is pretty much same, just wrapped up into the `Attributes` trait, so we can use it like this:

```scala
val keys = Attributes[User].fieldNames
// keys = List(id, name, surname)
```

# Scala 2.13 and productElementNames
[Scala 2.13] added new method, [productElementNames], into the [Product] trait, from which all _case classes_ inherits. This method returns list of all field names, so it can be used as alternative to the above code, unfortunately with one huge drawback. Because this is a method implemented in _trait_, you actually need instance of your _case class_ to call this method:

```scala
val user = User(123L, "John", "Smith")
val keys = user.productElementNames.toList
// keys: List[String] = List(id, name, surname) 
```

Because of this fact, it really cannot be used as replacement of the _Shapeless_ based implementation, but I decided to mention it here just for the sake of completeness.

[Aux pattern]: https://gigiigig.github.io/posts/2015/09/13/aux-pattern.html
[Bits of Shapeless part 1: HLists]: http://enear.github.io/2016/04/05/bits-shapeless-1-hlists/
[Product]: https://www.scala-lang.org/api/2.13.x/scala/Product.html
[productElementNames]: https://www.scala-lang.org/api/2.13.x/scala/Product.html#productElementNames:Iterator[String]
[Scala 2.13]: https://www.scala-lang.org/news/2.13.0
[shapeless]: https://github.com/milessabin/shapeless
[shapeless_Generic]: https://static.javadoc.io/com.chuusai/shapeless_2.13/2.3.3/shapeless/Generic.html
[shapeless_HList]: https://static.javadoc.io/com.chuusai/shapeless_2.13/2.3.3/shapeless/HList.html
[shapeless_Keys]: https://static.javadoc.io/com.chuusai/shapeless_2.13/2.3.3/shapeless/ops/record/Keys.html
[shapeless_LabelledGeneric]: https://static.javadoc.io/com.chuusai/shapeless_2.13/2.3.3/shapeless/LabelledGeneric.html
[The Type Astronaut's Guide to Shapeless]: https://underscore.io/books/shapeless-guide/