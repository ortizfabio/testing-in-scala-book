package bytetrend.test

import java.util.HashMap

import org.scalacheck.Prop._

import collection.JavaConversions._
import collection.immutable.IntMap
import org.scalacheck.Prop.{AnyOperators, forAll}
import org.scalacheck.{Prop, Properties}
import org.specs2.{ScalaCheck, Specification}
import org.specs2.control.Property

class IntMapSpecification  extends Specification with ScalaCheck
{
  override def is =
    s2"""
         Given a randomly generated IntMap
         When compare to a HashMap and filled with the same elements
         Then the size should be the same ${size}
         And if one is empty the other should also be empty ${empty}
    """
  def equalMaps(hm: HashMap[Int,Any], im: IntMap[Any]) = {
    im.keys.forall(hm.containsKey) &&
      hm.keySet.containsAll(im.keys) &&
      im.keys.forall(k => im(k) == hm(k))
  }

  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary.arbitrary

  val genMaps: Gen[(HashMap[Int,Any],IntMap[Any])] =
    arbitrary[List[Int]] map { xs =>
      val mappings = for(n <- xs) yield (n, new Object)
      val im = IntMap(mappings: _*)
      val hm = new HashMap[Int, Any]
      for((n,x) <- mappings) hm.put(n,x)
      (hm,im)
    }

 def size: Prop = forAll(genMaps) {
    case (hm, im) => im.size ?= hm.size
  }
  def empty: Prop = forAll(genMaps) {
    case (hm,im) => im.isEmpty ?= hm.isEmpty
  }
  def add: Prop = forAll(genMaps) {
    case (hm,im) => forAll { (k: Int, v: String) =>
      hm.put(k, v)
      equalMaps(hm, im + (k -> v))
    }
  }
}
