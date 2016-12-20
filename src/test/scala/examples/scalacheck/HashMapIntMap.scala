package examples.scalacheck

import org.specs2.control.Properties

/**
  * Created by jose.ortiz on 12/19/16.
  */
object HashMapIntMap extends App{
  import java.util.HashMap
  import collection.JavaConversions._
  import collection.immutable.IntMap

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

  //genMaps takes advantage of ScalaCheck's ability to automatically generate lists. We let ScalaCheck come up with a random list, and then create IntMap and HashMap instances from that list.
  //We can now create a property for each method we want to test, using genMaps to get comparable instances of both IntMap and HashMap:

  import org.scalacheck.Prop.{forAll, AnyOperators}
  import org.scalacheck.Properties

  object IntMapSpec extends Properties("IntMap")  {
    property("size") = forAll(genMaps) {
      case (hm, im) => im.size ?= hm.size
    }
    property("isEmpty") = forAll(genMaps) {
      case (hm,im) => im.isEmpty ?= hm.isEmpty
    }
    property("add") = forAll(genMaps) {
      case (hm,im) => forAll { (k: Int, v: String) =>
        hm.put(k, v)
        equalMaps(hm, im + (k -> v))
      }
    }
  }
  IntMapSpec.check()
}
