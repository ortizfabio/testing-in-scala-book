package bytetrend.test

import org.scalacheck.Gen
import org.scalacheck.Prop.{AnyOperators, forAll}
import org.scalacheck.{Prop, Properties}
import org.specs2.{ScalaCheck, Specification}

/**
  * Created by jose.ortiz on 10/20/16.
  */
class Examples extends Specification with ScalaCheck  {

  override def is =
    s2"""
         Given a randomly generated IntMap
         When compare to a HashMap and filled with the same elements
         Then the size should be the same ${vectorsProp}
         And if one is empty the other should also be empty ${treesProp}
    """

  val vectors: Gen[Vector[Int]] =
    for {
      x <- Gen.choose(-100, 100)
      y <- Gen.choose(-100, 100)
    } yield Vector(x, y)

  val intLength = 3

  val vectorsProp :Prop = forAll(vectors, intLength) { (v: Vector[Int], s: Int) =>  v.lengthCompare(s) == 0 }



trait Tree
case class Node(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

  val ints = Gen.choose(-100, 100)

  def leafs: Gen[Leaf] = for {
    x <- ints
  } yield Leaf(x)

  def nodes: Gen[Node] = for {
    left <- trees
    right <- trees
  } yield Node(left, right)

  def trees: Gen[Tree] = Gen.oneOf(leafs, nodes)

  val treesProp:Prop = forAll(trees){
    t => t.isInstanceOf[Node] == true
  }
}