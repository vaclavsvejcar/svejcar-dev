---
title: Troubles with Future
description: todo
tags: scala, monix, future
tableOfContents: true
---

todo

# What's wrong with Future?
Because of the way how _Future_ is designed, it might lead to some surprising situations. Below is the list of selected common problems.

## Breaks referential transparency
In general, referential transparency means that any expression can be replaced with its value without changing the program's behaviour. Such expression (or function) must be pure, meaning that is has no side effect, because any side effect would break this condition. Let's start with simple example:

```scala
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

val future = Future(println("hello"))
for {
    _ <- future
    _ <- future
    _ <- future
} yield ()
// hello
```

The above code prints the _hello_ string once. According to _referential transparency_, if the _Future_ is replaced by its value, the result should be the same. Let's see:

```scala
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

for {
    _ <- Future(println("hello"))
    _ <- Future(println("hello"))
    _ <- Future(println("hello"))
} yield ()
// hello
// hello
// hello
```

Now the _hello_ string is printed three times and it clearly breaks the _referential transparency_. Why is that? Because _Future_ is __eagerly evaluated__ and __memoizes__ it's value. That means that code inside _Future_ is evaluated right after it's defined, and it's evaluated only once, remembering the computed value. And the eager evaluation leads to another problem...

## ExecutionContext everywhere
Each _Future_ needs to know where to execute itself, on which _thread_. This is why the _ExecutionContext_ instance is required. In ideal world, it would be possible to define the Future value, perform some some transformations using `map`, `flatMap`, etc. and at the very end to call some kind of `run` method, which would run the entire chain using the _ExecutionContext_.

Unfortunately, because of the _eager_ nature of _Future_, the _ExecutionContext_ is required as implicit parameter by any of the transformation or callback methods, such as map, flatMap, foreach and onComplete. It basically means that anywhere in your codebase where you work with _Futures_, you have to somehow propagate also the _ExecutionContext_, which can become really cumbersome.

## Gotchas with for-comprehension
Sometimes it's required to execute several independent _Futures_ in parallel and combine their results into single value. Naive approach to this might be following:

```scala
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import Thread.sleep

def longRunningJob1: Int = {sleep(800); 2}
def longRunningJob2: Int = {sleep(300); 4}
def longRunningJob3: Int = {sleep(900); 6}

for {
    a <- Future(longRunningJob1)
    b <- Future(longRunningJob2)
    c <- Future(longRunningJob3)
} yield a + b + c
```

Problem is that this code actually runs __synchronously__. Why is that? If you desugar the _for-comprehension_, the code looks like this:

```scala
Future(longRunningJob1)
  .flatMap(a => Future(longRunningJob2)
    .flatMap(b => Future(longRunningJob3)
      .map(c => a + b + c)))

```

And the `flatMap` method on _Future_ is executed __after__ the _Future's_ value is evaluated, this is also clearly stated in the method's _Scaladoc_:

> Creates a new future by applying a function to the successful result of this future, and returns the result of the function as the new future.

The workaround for this is to declare the _Futures_ before merging them together, like this:

```scala
val futureA = Future(longRunningJob1)
val futureB = Future(longRunningJob2)
val futureC = Future(longRunningJob3)

for {
    a <- futureA
    b <- futureB
    c <- futureC
} yield a + b + c
```

Main problem with this behaviour is that the __code behaves differently__ based on how it's structured. Now let's check if there's better alternative to Scala _Futures_.

# Monix Task to the rescue

# Conclusion


