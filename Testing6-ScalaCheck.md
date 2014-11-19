# Testing with ScalaTest and ScalaCheck

## [ScalaCheck](http://www.scalacheck.org/)
* Gen: generate random test data
* Prop: specify SUT properties ("laws", "rules", "invariants")
* [User Guide](https://github.com/rickynils/scalacheck/wiki/User-Guide)

### [Gen](http://www.scalacheck.org/files/scalacheck_2.11-1.11.6-api/#org.scalacheck.Gen)
* built-in generators for most Scala standard types: Boolean, String, Int, List, Option, ...  
[Example: random Ints](exercises/src/test/scala/fpinscala/gettingstarted/GettingStartedSpec.scala#L101)  
[Example: random Lists of Ints](exercises/src/test/scala/fpinscala/monoids/MonoidSpec.scala#L116)
* build your own generators with provided [Gen combinators](http://www.scalacheck.org/files/scalacheck_2.11-1.11.6-api/#org.scalacheck.Gen)  
[Example: custom generators](exercises/src/test/scala/fpinscala/monoids/MonoidSpec.scala#L163)  
[Example: recursive custom generators](exercises/src/test/scala/fpinscala/datastructures/TreeSpec.scala#L15)
([Arbitrary](http://www.scalacheck.org/files/scalacheck_2.11-1.11.6-api/#org.scalacheck.Arbitrary)
makes a generator usable like standard Scala types)  
Real World Example: DataStore smoketestclient generators

### [Prop](http://www.scalacheck.org/files/scalacheck_2.11-1.11.6-api/#org.scalacheck.Prop)
* create Prop with [forAll](https://github.com/rickynils/scalacheck/blob/master/src/main/scala/org/scalacheck/Prop.scala#L736)
* combine Props with combinators  
[Example: Prop && combinator](exercises/src/test/scala/fpinscala/testing/GenSpec.scala#L45)

### Goodies
* Shrinking: ScalaCheck tries to reduce the failure test data to the smallest possible size
* Stateful testing  
[Example: Candy Dispenser][Example: Candy Dispenser rules](exercises/src/test/scala/fpinscala/state/StateSpec.scala#L333)

[<< Property-based Testing](Testing5-PropertyBasedTesting.md)
