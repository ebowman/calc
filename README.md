Trivial program to brute-force solve the brilliant.org problem found here: 

https://brilliant.org/practice/arithmetic-puzzles-level-2-challenges/?p=2

You'll need [sbt](http://www.scala-sbt.org/). There are basically only three things you could imagine doing in
such a simple project, clean, test and run:

````
$ sbt
[info] Loading global plugins from /Users/ebowman/.sbt/0.13/plugins
[info] Loading project definition from /Users/ebowman/src/ebowman/calc/project
[info] Set current project to calc (in build file:/Users/ebowman/src/ebowman/calc/)
> clean
[success] Total time: 0 s, completed 18-Feb-2017 20:39:35
> test
[info] Updating {file:/Users/ebowman/src/ebowman/calc/}calc...
[info] Resolving jline#jline;2.14.1 ...
[info] Done updating.
[info] Compiling 2 Scala sources to /Users/ebowman/src/ebowman/calc/target/scala-2.12/classes...
[info] Compiling 1 Scala source to /Users/ebowman/src/ebowman/calc/target/scala-2.12/test-classes...
[info] OpsSpec:
[info] Numbers
[info] - should be parsable
[info] Terms
[info] - should be parsable
[info] Expressions
[info] - should be parsable
[info] Run completed in 707 milliseconds.
[info] Total number of tests run: 3
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 3, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[success] Total time: 20 s, completed 18-Feb-2017 20:39:57
> run
[info] Running calc.Calc 
For 1 _ 2 _ 3 _ 4, there are 2 solutions:
1 + 2 + 3 + 4
1 * 2 * 3 + 4
For 1 _ 2 _ 3 _ 4 _ 5, there are 3 solutions:
1 + 2 + 3 + 4 + 5
1 - 2 * 3 + 4 * 5
1 * 2 * 3 + 4 + 5
[success] Total time: 0 s, completed 18-Feb-2017 20:39:59
````

  
  