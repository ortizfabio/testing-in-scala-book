package scala.examples

import org.specs2.specification.script.StandardDelimitedStepParsers
import org.specs2.mutable.Specification
import org.specs2.specification.dsl.mutable.GWT

/**
  * Created by jose.ortiz on 8/3/16.
  */
class GWTSpecMutable extends Specification
  with GWT with StandardDelimitedStepParsers {

  "adding numbers".p

  step("Given a first number {2}")(anInt) { i =>
    number = i
  }

  step("When I multiply it by {3}")(anInt) { j =>
    number = number * j
  }

  example("Then I get {6}")(anInt) { n: Int =>
    number must_== n
  }

  var number = 0
}
