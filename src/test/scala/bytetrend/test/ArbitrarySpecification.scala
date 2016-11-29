package bytetrend.test

import org.scalacheck.Prop._
import org.scalacheck.Prop.{AnyOperators, forAll}
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.specs2.{ScalaCheck, Specification}
import cats._
import cats.data.Validated
import cats.implicits._



class ArbitrarySpecification extends Specification with ScalaCheck
{
  override def is =
    s2"""
  a simple property       $ex1
  a more complex property $ex2
  """
  def abStringGen = (Gen.oneOf("a", "b")) //|@| Gen.oneOf("a", "b"))(_+_)

  implicit def abStrings: Arbitrary[String] =
    Arbitrary(abStringGen)

  def ex1 = prop((s: String) => s must contain("a") or contain("b")).setArbitrary(abStrings)

  // use the setArbitrary<n> method for the nth argument
  def ex2 = prop((s1: String, s2: String) => (s1+s2) must contain("a") or contain("b")).
    setArbitrary1(abStrings).setArbitrary2(abStrings)
}
