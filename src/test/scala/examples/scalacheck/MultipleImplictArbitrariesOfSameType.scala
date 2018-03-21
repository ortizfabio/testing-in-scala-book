package examples.scalacheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.{forAll, _}
import org.scalacheck.Shrink.shrinkAny
import org.scalacheck.util.Pretty.prettyAny
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

/**
  * https://groups.google.com/forum/#!topic/scalacheck/YWZGn0Wrtws
  * This two implicits produce an error
  */
object TwoArbitraryImplictProduceError {

  case class Foo(x: Any)

  val fooGen: Gen[Foo] = arbitrary[Int].map(Foo(_))
  implicit val arbitraryFoo: Arbitrary[Foo] = Arbitrary(fooGen)
  forAll { _: Foo => (true == true) }

  val fooGenDouble: Gen[Foo] = arbitrary[Double].map(Foo(_))
  //This produces error ambiguous implicit values:
  // both value arbitraryFoo in object UsingGenerators of type => org.scalacheck.Arbitrary[examples.scalacheck.UsingGenerators.Foo]
  // and value arbitraryFooDouble in object UsingGenerators of type => org.scalacheck.Arbitrary[examples.scalacheck.UsingGenerators.Foo]
  // match expected type org.scalacheck.Arbitrary[examples.scalacheck.UsingGenerators.Foo]
  forAll { _: Foo => (true == true) }
  //  implicit val arbitraryFooDouble: Arbitrary[Foo] = Arbitrary(fooGenDouble)
  forAll { _: Foo => (true == true) }
}

/**
  * This example is the same as above but now the implicit work because they
  * are of differnt type.
  */
object TwoArbitraryImplictUseType {

  case class Foo[T](x: T)

  val fooGen: Gen[Foo[Int]] = arbitrary[Int].map(Foo[Int](_))
  implicit val arbitraryFooInt: Arbitrary[Foo[Int]] = Arbitrary(fooGen)
  forAll { _: Foo[Int] => (true == true) }

  val fooGenDouble: Gen[Foo[Double]] = arbitrary[Double].map(Foo(_))
  implicit val arbitraryFooDouble: Arbitrary[Foo[Double]] = Arbitrary(fooGenDouble)
  forAll { _: Foo[Double] => (true == true) }

}

/**
  * The best solution in this case you can always skip using Arbitrary
  * and simply specify your generator explicitly
  */
object TwoArbitraryImplictFinalSolution {

  case class Foo(x: Any)

  val fooGen: Gen[Foo] = arbitrary[Int].map(Foo(_))
  //implicit val arbitraryFoo: Arbitrary[Foo] = Arbitrary(fooGen)
  forAll(fooGen) { _: Foo => (true == true) }

  val fooGenDouble: Gen[Foo] = arbitrary[Double].map(Foo(_))
  //implicit val arbitraryFooDouble: Arbitrary[Foo] = Arbitrary(fooGenDouble)
  forAll(fooGenDouble) { _: Foo => (true == true) }

}

object TwoArbitraryImplictAlternateSolution {

  case class Foo(x: Any)

   class TwoArbitraryExplicitSolution extends Properties("Testing") {

    val fooGen: Gen[Foo] = arbitrary[Int].map(Foo(_))
   val arbitraryFoo: Arbitrary[Foo] = Arbitrary(fooGen)
    val f1 =  (_: Foo) => true == true
     //The original and right way would be to have (_) => this
     //But for some reason does not work.
    val f11:(Boolean=>Prop) = {(_:Boolean) => proved}
    property("test1") = forAll {
      f1
    }(f11, arbitraryFoo, shrinkAny, prettyAny)

    val fooGenDouble: Gen[Foo] = arbitrary[Double].map(Foo(_))
    //This produces error ambiguous implicit values:
    // both value arbitraryFoo in object UsingGenerators of type => org.scalacheck.Arbitrary[examples.scalacheck.UsingGenerators.Foo]
    // and value arbitraryFooDouble in object UsingGenerators of type => org.scalacheck.Arbitrary[examples.scalacheck.UsingGenerators.Foo]
    // match expected type org.scalacheck.Arbitrary[examples.scalacheck.UsingGenerators.Foo]
//    forAll { _: Foo => (true == true) }
    val arbitraryFooDouble: Arbitrary[Foo] = Arbitrary(fooGenDouble)
    val f2 = ( _: Foo) => (true == true)
     //The original and right way would be to have (_) => this
     //But for some reason does not work.
    val f22 = {(_:Boolean) => proved}
    property("test2") = forAll {
      f2
    }( f22, arbitraryFooDouble, shrinkAny, prettyAny)
  }
}
