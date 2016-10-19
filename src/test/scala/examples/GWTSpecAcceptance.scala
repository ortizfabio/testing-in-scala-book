package scala.examples

import org.specs2.{Specification, specification}
import org.specs2.specification.script.StandardDelimitedStepParsers

/**
  * Created by jose.ortiz on 8/3/16.
  */
class GWTSpecAcceptance extends Specification with specification.dsl.GWT
  with StandardDelimitedStepParsers {

  def is = s2"""
 Given a first number {2}     $g1
 When multiply it by {3}      $w1
 Then I get {6}               $t1
"""
  var number = 0
  def g1 = step(anInt) {
    i => number = i
  }

  def w1 = step(anInt) {
    j => number = number * j
  }

  def t1 = example(anInt) {
    n => number must_== n
  }
}

