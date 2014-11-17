# Testing with ScalaTest and ScalaCheck

## [(Generator-driven) Property-based Testing](http://scalatest.org/user_guide/generator_driven_property_checks)
* idea: let the test system automagically generate random test data
* specify "Properties" on SUT: general "rules" or "laws" that our code should obey
* a law is of the form "for all x1,x2,...: f(SUT,x1,x2,...)"  
  [Example: tail and head of List](exercises/src/test/scala/fpinscala/datastructures/ListSpec.scala#L61)

### Advantages of Property-based Testing
* impartial: no (subconscious?) urge to "protect" your own code from bug detection
* potential to detect more bugs due to unforeseen input data
* laws as a kind of additional specification/documentation

[<< Hand-made Testing](Testing4-HandMadeTesting.md)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[ScalaCheck >>](Testing6-ScalaCheck.md)