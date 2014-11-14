# Testing with ScalaTest and ScalaCheck

## Why tests?
* find bugs
* "prove" correctness (= meets expectations/specifications)
* specify behavior (test first, TTD)
* document and communicate behavior
* fixate behavior (e.g. before refactoring)
* avoid future regressions
* explore unknown code
* good feeling (sleep better)

## [ScalaTest](http://scalatest.org/)
* DSL for "human readable" tests: [assertions](http://scalatest.org/user_guide/using_assertions),
[matchers](http://scalatest.org/user_guide/using_matchers) and
* different [testing styles](http://scalatest.org/user_guide/selecting_a_style):  
FlatSpec, FeatureSpec, FreeSpec, WordSpec, FunSuite, GivenWhenThen, WordSpec, ...
* "If you don't enjoy shopping: ... use [FlatSpec](http://doc.scalatest.org/2.2.1/#org.scalatest.FlatSpec)
for unit and integration testing
and [FeatureSpec](http://doc.scalatest.org/2.2.1/#org.scalatest.FeatureSpec) for acceptance testing."  
-> fair enough enough for me

## What is a test?
* [System Under Test](http://en.wikipedia.org/wiki/System_under_test) (SUT):
function, class, module, ...
* input -> SUT -> expected vs. actual output
* problem: how to find good (and sufficient!?) input data? when are we done?
*  Edsger W. Dijkstra: "Program testing can be used to show the presence of bugs,
but never show their absence!"

## How can we test?
### Hand-made Testing
* come up with possible input data: typical and corner cases; exceptional/"irregular" cases
* ScalaTest: individual single tests or
* [table-driven property tests](http://scalatest.org/user_guide/property_based_testing):
more compact, same syntax
* problem: sufficient coverage? all "representative" cases? how can we be sure?

### [Property-based Testing](http://scalatest.org/user_guide/property_based_testing)
* idea: let the test system generate itself random test data
* specify only general "rules" or "laws" that our code should obey
* "for all x,y,z: f(SUT,x,y,z)"
* advantages: detect additional bugs, laws as a kind of specification/documentation

## [ScalaCheck](http://www.scalacheck.org/)
* Gen(enerators) for test data
* Prop(erties) for specifying "laws"