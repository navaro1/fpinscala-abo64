# Testing with ScalaTest and ScalaCheck

## What is a test?
* [System Under Test](http://en.wikipedia.org/wiki/System_under_test) (SUT):
function, class, module, application, ...
* test: input -> SUT -> output -> check output against expectations
* problem: how to find good (and sufficient!?) input data? when are we done?
* Edsger W. Dijkstra: "Program testing can be used to show the presence of bugs,
but never show their absence!" -> no correctness or verification (at least for nontrivial SUTs)
* TL;DNR: quality of a test = quality of its input data!

How can we write good tests? => How do we find good input data?

[<< ScalaTest DSL](Testing2-ScalaTestDSL.md)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[Hand-made Testing >>](Testing4-HandMadeTesting.md)