# Testing with ScalaTest and ScalaCheck

## Hand-made Testing
* idea: come up with possible input data yourself
* typical and corner cases; exceptional/"irregular" cases
* ScalaTest: individual single tests  
[Example: typical behavior](exercises/src/test/scala/fpinscala/datastructures/ListSpec.scala#L35)  
[Example: exceptional behavior](exercises/src/test/scala/fpinscala/datastructures/ListSpec.scala#L39)
* [table-driven property tests](http://scalatest.org/user_guide/table_driven_property_checks):  
[Example: table-driven test](exercises/src/test/scala/fpinscala/datastructures/ListSpec.scala#L48)  
[Example: Candy Dispenser rules](exercises/src/test/scala/fpinscala/state/StateSpec.scala#L301)
* more compact than single tests (cf. [TestNG's DataProvider](http://testng.org/doc/documentation-main.html#parameters-dataproviders))
* syntax similar to generator-driven property tests
* but usually contains both input and expected output data

### Houston, we have a problem
* all "representative" cases?
* sufficient coverage?
* how can we be sure?

[<< What is a Test?](Testing3-WhatIsATest.md) [Property-based Testing >>](Testing5-PropertyBasedTesting.md)