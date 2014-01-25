# Grokking SBT

<img alt="confusion" align=right src="sbt.gif"/>

After a great deal of trying, I just had to accept that the standard doc, the source, and even the upcoming book were not sufficient in breaking through my apparent total inability to understand how SBT works. So in an attempt to force my own understanding I'm writing my own doc.

I enthusiastically invite criticism and corrections. The point is to produce another perspective on this stuff that may help others who may share my apparently unusual learning process. So please find me on Twitter or #scala if you'd like to chat.

rob / `tpolecat`

### Itended Audience

I will assume you are smart, yet you have been unable to grok SBT. This may be a small set.

### TODO


- "You need to specify the scope if the key in question is normally scoped. For example, the compile task, by default, is scoped to Compile and Test configurations, and does not exist outside of those scopes." yet you can inspect compile and it works. it picks the compile config but i can't figure out by what logic it does this
neither do i
- how can i refer to a key in another project? or can you? packageOptions in (Compile, packageBin) is ambiguous in a multi-project build
- the doc talks about three axes: project, config, task … what is the build uri? is it constant or another axis? if it's constant why does it exist?
- does {.}/* in delegates mean the same thing as */* in inspect tree?
- what is the difference between Global and ThisBuild?
- where are the parsers defined? the complete package/object doesn't seem to exist

### Big (but accurate) Picture

This is how SBT works. It is very simple. Cling to this.

- An SBT build is a set of **keys** bound to computations that produce values. 
- These computations can depend on other keys, forming a directed dependency graph.
- We "do things" in SBT by **forcing** these computations, which perform work as a side-effect.

This is what it means.

- Using SBT is an exercise in associating computations with keys. That's basically it.
- There are bunch of predefined keys with default computations, so for simple projects there's not a lot of work to do.

Ok so let's look at one of these keys and its associated computation to make things more concrete. 

> **ASIDE**: This is the kind of digression that I require in order to feel comfortable with an idea. I want to know specifically (not in a hand-wavey way) what we're actually talking about. So I'm going to take a few minutes and look at one of the default keys and what it's actually doing. By looking at one example in detail we'll know how to look at any other key we find interesting.

Ok, so when you type `compile` at the SBT prompt, you are asking SBT to find the key with the label "compile" and compute its value. As a side-effect of this computation your source files get compiled. But what kind of value does it compute?

Let's find out what `compile` computes. Go to an empty directory and type `sbt` and we'll play around with a totally empty build. 

##### Inspecting a Key

At the SBT prompt if you `inspect compile` it will give you a rather long summary of the key with the label "compile". Let's just look at the top first:

```
> inspect compile
[info] Task: sbt.inc.Analysis
[info] Description:
[info]  Compiles sources.
...
```

So this first line tells us that "compile" is the label for a key of type `TaskKey[sbt.inc.Analysis]`. Keys are parameterized on the type of the computed result, so the end product of this computation is an `Analysis`, whatever that is. We can confirm by looking at the key itself in the REPL. At the SBT prompt type `reload plugins` (which just gives us a way to see SBT itself in the REPL) and then `console`.

```
scala> val k = sbt.Keys.compile
k: sbt.TaskKey[sbt.inc.Analysis] = sbt.TaskKey$$anon$3@12a40ad

scala> k.key.label
res0: String = compile

scala> k.key.description
res1: Option[String] = Some(Compiles sources.)
```

So we see that indeed the key with the label "compile" has the type we gleaned from `inspect` above. As an aside, most (all?) of the keys you get for free are defined in `sbt.Keys` so it's a useful class to explore.

##### Return Value

So what is this `Analysis` value? Let's look. (I'm doing this in a project with some classes, otherwise you don't get a useful result.)

```
> compile
[success] Total time: 0 s, completed Jan 17, 2014 4:42:16 PM
```

Well, what the hell. SBT *doesn't even show us the result*. This is telling; we don't see the result because usually *it doesn't matter*. Most of the time we are forcing a computation purely for its side-effects and SBT doesn't bother to show the end result because we rarely actually care (although *other computations* might care of course).

We can tell SBT we actually want to see the result with `show`.

```
> show compile
[info] Analysis: 46 Scala sources, 1519 classes, 9 binary dependencies
[success] Total time: 0 s, completed Jan 17, 2014 4:44:47 PM
```

Ok, not super useful but presumably this object has information that's useful to anyone who depends on this task. But `compile` is not a monolithic computation; it builds up its result in part by asking *other* keys for their values. Let's look at this in some detail.


##### Dependency Graph

We can see the other computations that `compile` depends on in several ways. Back in our empty project let's `inspect compile` again, and look at the **Dependencies** section:

```
> inspect compile
...
[info] Dependencies:
[info]  compile:compile::compileInputs
[info]  compile:compile::streams
[info] Reverse dependencies:
...
```

Let's forget about the `foo:bar::` prefixes for now (these denote **scopes** which we'll get to soon). It looks like `compile` depends on `compileInputs` and `streams`, which may then depend on other keys themselves. We can look at the whole tree thus:

```
> inspect tree compile
[info] compile:compile = Task[sbt.inc.Analysis]
[info]   +-compile:compile::compileInputs = Task[sbt.Compiler$Inputs]
[info]   | +-compile:dependencyClasspath = Task[scala.collection.Seq[sbt.Attributed[java.io.File]]]
[info]   | +-*:compilers = Task[sbt.Compiler$Compilers]
[info]   | +-*/*:sourcePositionMappers = Task[scala.collection.Seq[scala.Function1[xsbti.Position, scala.Option[xsbti.Position]]]]
[info]   | +-*/*:compileOrder = Mixed
[info]   | +-compile:scalacOptions = Task[scala.collection.Seq[java.lang.String]]
[info]   | +-compile:sources = Task[scala.collection.Seq[java.io.File]]
[info]   | +-*/*:maxErrors = 100
[info]   | +-compile:incCompileSetup = Task[sbt.Compiler$IncSetup]
[info]   | +-compile:classDirectory = target/scala-2.10/classes
[info]   | +-compile:compile::streams = Task[sbt.std.TaskStreams[sbt.Init$ScopedKey[_ <: Any]]]
[info]   | | +-*/*:streamsManager = Task[sbt.std.Streams[sbt.Init$ScopedKey[_ <: Any]]]
[info]   | | 
[info]   | +-*/*:javacOptions = Task[scala.collection.Seq[java.lang.String]]
[info]   | 
[info]   +-compile:compile::streams = Task[sbt.std.TaskStreams[sbt.Init$ScopedKey[_ <: Any]]]
[info]     +-*/*:streamsManager = Task[sbt.std.Streams[sbt.Init$ScopedKey[_ <: Any]]]
[info]     
```

Observe that some labels are bound to `Task` objects and others are bound to readable values. Here we see a distinction: there are **two** kinds of computations:

 - **Tasks** are evaluated on demand, and thus can respond to changes in the environment (i.e., source files).
 - **Settings** are evalated once, when the project is loaded.

It may or may not be helpful to think: "Task is to Setting as `def` is to `val`." Note that the semantics here implies that tasks can depend on settings, but not vice-versa. 

Ok now let's figure out what those long names mean.

##### Scopes

The general form for keys is:

    {<build-uri>}<project-id>/config:intask::key

The project uri seems to be constant and irrelevant. The project, config, and task are referred to as "axes", and a key might be associated with different computations for different combinations of axis values. A missing value for project or config means "current" and a `*` means "any". It's not clear what "current" config means.

You can see this full form for a given key with `inspect`:

```
> inspect compile
...
[info] Provided by:
[info]  {file:/private/tmp/test/}test/compile:compile
...
```

So what we see here is *one of* the instances of the `compile` key. It turns out that a key can be defined in multiple scopes, and if you `inspect` a key you get, um, one of them. It's not at all clear which one you get. If you want to see all of them you can say `inspect definitions`:

```
> inspect definitions compile
[info] 
[info]  compile:compile
[info]  test:compile
```

and then `inspect` again with a more specific key:

```
> inspect test:compile
...
[info] Provided by:
[info]  {file:/private/tmp/test/}test/test:compile
...
```










