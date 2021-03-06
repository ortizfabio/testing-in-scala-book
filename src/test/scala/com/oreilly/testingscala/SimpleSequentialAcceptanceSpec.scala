package com.oreilly.testingscala

import org.specs2.Specification


import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
@RunWith(classOf[JUnitRunner])
class SimpleSequentialAcceptanceSpec extends Specification { def is =
    args(sequential = true)               ^
  "This is a simple specification"      ^
      "and this should run f1"          ! f1 ^
      "and this example should run f2"  ! f2

    def f1 = success
    def f2 = pending
}