---
title: Troubles with Future
description: Scala's Future provides easy way to run code asynchronously, but also has some pitfals. Let's discuss the biggest issues and compare Future to Monix Task.
tags: scala, monix, future
tableOfContents: true
---

Scala's [Future] is integral part of the standard library and probably anyone who needs to execute some code asynchronously used it at least once. It's also used by many and many popular libraries, such as [Apache Spark], [Akka] or [Slick]. However, because of the way how it's designed, its use can lead to some surprising situations. This blog post summarizes some pitfalls and introduces [Monix Task][Task] as more powerful alternative.

<!-- MORE -->

# So what's wrong with Future?
_NOTE: I know that saying about something that it's wrong is pretty objective, so let's make it clear that by wrong I mean wrong mainly from the perspective of functional programming._

Scala's `Future` represents a value, that might not be currently available, but will be at some point of time. Current design of `Future` follows these two important aspects:

- __eager evaluation__ - `Future` starts evaluating its value right after it's defined
- __memoization__ - once the value is computed, it's shared to anyone who asks for it, without being recalculated again



From these two points it might be immediately obvious that such design decisions might lead to some surprising results, summarized in following chapters.

## Breaks referential transparency
In general, [referential transparency][wiki:referential_transparency] means that any expression can be replaced with its value without changing the program's behaviour. Such expression (or function) must be [pure][wiki:pure_function], meaning that is has no [side effect][wiki:side_effect], because any side effect would break this condition. Let's start with simple example:

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

The above code prints the _hello_ string once. According to _referential transparency_, if the `Future` is replaced by its value, the result should be the same. Let's see:

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

Now the _hello_ string is printed three times and it clearly breaks the _referential transparency_. Why is that? Because `Future` is __eagerly evaluated__ and __memoizes__ it's value. That means that code inside `Future` is evaluated right after it's defined, and it's evaluated only once, remembering the computed value. And the eager evaluation leads to another problem...

## ExecutionContext everywhere
Each `Future` needs to know where to execute itself, on which _thread_. This is why the [ExecutionContext] instance is required. In ideal world, it would be possible to define the Future value, perform some some transformations using `map`, `flatMap`, etc. and at the very end to call some kind of `run` method, which would run the entire chain using the `ExecutionContext`.

Unfortunately, because of the _eager_ nature of `Future`, the `ExecutionContext` is required as implicit parameter by any of the transformation or callback methods, such as [map][scaladoc:Future#map], [flatMap][scaladoc:Future#flatMap], [foreach][scaladoc:Future#foreach] and [onComplete][scaladoc:Future#onComplete]. It basically means that anywhere in your codebase where you work with `Future`s, you have to somehow propagate also the `ExecutionContext`, which can become really cumbersome.

## Gotchas with for-comprehension
Sometimes it's required to execute several independent `Future`s in parallel and combine their results into single value. Naive approach to this might be following:

```scala
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
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

And the `flatMap` method on `Future` is executed __after__ its value is evaluated, this is also clearly stated in the method's _Scaladoc_:

> Creates a new future by applying a function to the successful result of this future, and returns the result of the function as the new future.

The workaround for this is to declare the `Future`s before merging them together, like this:

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

This works beacuse of the _eager evaluation_ nature of `Future`. The computation of values for fields `futureA`, `futureB` and `futureC` starts independently before the `for` block is performed. The problem here is that the code behaves differently based on how it's structured and code author must be aware of this. If `Future` was _lazily evaluated_, both examples would behave the same.

# Monix Task to the rescue
[Monix][web:monix] is popular _Scala_ library, providing various tools for composing asynchronous programs. One of the data type provided by this library is [Task], representing (possibly) asynchronous computation. Here is the overview of key architecture differences between `Task` and `Future`:

|            | evaluation | memoization               |
|------------|------------|---------------------------|
| __Future__ | _eager_    | _yes (forced)_            |
| __Task__   | _lazy_     | _no (but can be enabled)_ |

Using `Task` is very similar to using `Future`. Main difference is that instead of `ExecutionContext`, you need the [Scheduler][scaladoc:Scheduler] (which is basically just wrapper around it), but you need it only at point when you need to actually evaluate the Task:

```scala
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

// 1) define the task
val task1 = Task(println("hello"))

// 2) then run it
task1.runSyncUnsafe() // executes the task, synchronously (blocking operation)
```

_Monix_ also provides fine grained control over how the `Task` will be executed. By using various implementations of `Scheduler`, you can choose _where_ the `Task` will be executed (fixed thread pool, etc.) and using the various `runXY` methods, you can tell _how_ the task will be executed (synchronously, asynchronously, with delay, etc). See the [official documentation][monix-documentation] for more details.

Let's now check if using `Task` instead of `Future` can solve the issues we had above.

## Preserves referential transparency
As shown earlier, `Future` breaks rules of _referential transparency_, which might lead to some surprising errors, mainly if `Future` performs some _side effects_. Let's compare it with `Task`.

```scala
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

val task = Task(println("hello"))
val result = for {
  _ <- task
  _ <- task
  _ <- task
} yield ()

result.runSyncUnsafe()
// hello
// hello
// hello
```

The above code prints the string to console three times, because `Task` does not memoize the computed value, so each time it's used its value is computed again. Let's see what happens if we replace the `task` variable references by inlining the `Task` itself.

```scala
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

val result = for {
  _ <- Task(println("hello"))
  _ <- Task(println("hello"))
  _ <- Task(println("hello"))
} yield ()

result.runSyncUnsafe()
// hello
// hello
// hello
```

And output is the same for both of the examples, which means that `Task` preserves _referential transparency_.

## Scheduler needed only for evaluation
One of the ugly properties of `Future` is that methods used for value manipulation, such `map` and `flatMap`, requires implicit value of `ExecutionContext` to be in scope, so you need to propagate it through your codebase. `Task` requires sheduler only for its execution using the `runXY` methods, so there's no need to pollute your codebase with its instances.

## Consistent behaviour with for-comprehension
Earlier we discussed that using multiple values of `Future` in _for-comprehension_ might result in different execution, based on how the source code is structured. Let's compare it with the same example, rewritten using the `Task`:

```scala
def longRunningJob1: Int = { sleep(800); println("executing job 1"); 2 }
def longRunningJob2: Int = { sleep(300); println("executing job 2"); 4 }
def longRunningJob3: Int = { sleep(900); println("executing job 3"); 6 }

val result = for {
  a <- Task(longRunningJob1)
  b <- Task(longRunningJob2)
  c <- Task(longRunningJob3)
} yield a + b + c

result.runSyncUnsafe()
```

Written this way, these three _Tasks_ are executed synchronously, because the _for-comprehension_ is again desugared into `flatMap` calls and `flatMap` waits until the result of previous `Task` is computed. So far it's pretty same to the `Future`. Let's see what happens if the `Task` values are defined outside the `for` block:

```scala
  val task1 = Task(longRunningJob1)
  val task2 = Task(longRunningJob2)
  val task3 = Task(longRunningJob3)


  val result = for {
    a <- task1
    b <- task2
    c <- task3
  } yield a + b + c

  result.runSyncUnsafe()
```

And, unlike `Future`, the result is the same, _Tasks_ are again executed synchronously. This is because unlike `Future`, `Task` is always _lazily evaluated_ so it really doesn't matter where in the code you define it. If you want to execute multiple `Task` values in parallel, you have to explicitly do that on your own (see documentation about [parallel processing][monix-parallel_processing]).

# Conclusion
As shown in simple examples in above article, [Future]'s design, mainly the _eager evaluation_ and _memoization_ can lead to some unexpected situations, mainly when there's need to use it in functional codebase. [Monix Task][Task] is nice alternative that preserves some fundamental principles of _functional programming_, such as _referential transparency_, allows to write more clean codebase by reducing the need of `ExecutionContext` everywhere and provides more fine grained control over _where_ and _how_ it's executed.

[Akka]: https://akka.io/
[Apache Spark]: https://spark.apache.org/
[ExecutionContext]: https://www.scala-lang.org/api/current/scala/concurrent/ExecutionContext.html
[Future]: https://www.scala-lang.org/api/currentscala/concurrent/Future.html
[Slick]: http://slick.lightbend.com/
[Task]: https://monix.io/api/3.0/monix/eval/Task.html
[monix-documentation]: https://monix.io/docs/3x/#monix-execution
[monix-parallel_processing]: https://monix.io/docs/3x/tutorials/parallelism.html
[scaladoc:Future#flatMap]: https://www.scala-lang.org/api/current/scala/concurrent/Future.html#flatMap[S](f:T=%3Escala.concurrent.Future[S])(implicitexecutor:scala.concurrent.ExecutionContext):scala.concurrent.Future[S]
[scaladoc:Future#foreach]: https://www.scala-lang.org/api/current/scala/concurrent/Future.html#foreach[U](f:T=%3EU)(implicitexecutor:scala.concurrent.ExecutionContext):Unit
[scaladoc:Future#map]: https://www.scala-lang.org/api/current/scala/concurrent/Future.html#map[S](f:T=%3ES)(implicitexecutor:scala.concurrent.ExecutionContext):scala.concurrent.Future[S]
[scaladoc:Future#onComplete]: https://www.scala-lang.org/api/current/scala/concurrent/Future.html#onComplete[U](f:scala.util.Try[T]=%3EU)(implicitexecutor:scala.concurrent.ExecutionContext):Unit
[scaladoc:Scheduler]: https://monix.io/api/3.0/monix/execution/Scheduler.html
[web:monix]: https://monix.io/
[wiki:pure_function]: https://en.wikipedia.org/wiki/Pure_function
[wiki:referential_transparency]: https://en.wikipedia.org/wiki/Referential_transparency
[wiki:side_effect]: https://en.wikipedia.org/wiki/Side_effect_(computer_science)


