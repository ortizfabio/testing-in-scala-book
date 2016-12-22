package examples.scalacheck

import org.scalacheck.Prop.forAll
import org.scalacheck.{Properties, Test}

/**
  * Created by jose.ortiz on 12/20/16.
  */
object CustomGeneratorSample {
  import org.scalacheck.Arbitrary.arbitrary
  import org.scalacheck.Gen.choose

  val genStringWithN = for {
    s <- arbitrary[String]
    n <- choose(0, s.length)
  } yield (s,n)
  //We can now specify the property with only one forAll call:

  import org.scalacheck.Prop.forAll

  val genPrefix = forAll(genStringWithN) { case (s,n) =>
    val prefix = s.substring(0, n)
    s.startsWith(s)
  }

}

object NestedGeneratorSample extends Properties("nexted forall"){

  import org.scalacheck.Gen.choose
  import org.scalacheck.Prop.forAll

  val propPrefix = forAll { s: String =>
    forAll(choose(0, s.length)) { n =>
      val prefix = s.substring(0, n)
      s.startsWith(s)
    }
  }

}

object CaseClassGeneratorSample extends Properties("case class"){
  import org.scalacheck.Gen.{choose, oneOf}

  case class Person (
                      firstName: String,
                      lastName: String,
                      age: Int
                    ) {
    def isTeenager = age >= 13 && age <= 19
  }

  val genPerson = for {
    firstName <- oneOf("Alan", "Ada", "Alonzo")
    lastName <- oneOf("Lovelace", "Turing", "Church")
    age <- choose(1,100)
  } yield Person(firstName, lastName, age)

  import org.scalacheck.Arbitrary
  import org.scalacheck.Prop.forAll

  //This generates an random person.
  implicit val arbPerson:Arbitrary[Person] = Arbitrary(genPerson)

  val propPerson = forAll { p: Person =>
    p.isTeenager == (p.age >= 13 && p.age <= 19)
  }

}

object MathProps extends Properties("java.lang.Math") {

  property("max") = forAll { (x: Int, y: Int) =>
    val z = java.lang.Math.max(x, y)
    (z == x || z == y) && (z >= x && z >= y)
  }
}
object ScalaSampleRun extends App {
  import CustomGeneratorSample._

  MathProps.check(Test.Parameters.default)
  NestedGeneratorSample.check()
  NestedGeneratorSample.propPrefix.check
  genPrefix.check
  CaseClassGeneratorSample.check()
  CaseClassGeneratorSample.propPerson.check
}
