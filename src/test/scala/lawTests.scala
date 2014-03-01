package edu.luc.cs.cs372.expressionsAlgebraic

import org.scalatest.FunSuite
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import scalamu._

class lawTests extends FunSuite {

  import structures._
  import structures.ExprFactory._

  test("equality works") {
    (Constant(3): ExprF[Int]) assert_=== (Constant(3): ExprF[Int])
    constant(3) assert_=== constant(3)
    uminus(constant(3)) assert_=== uminus(constant(3))
  }

  test("equality and functor laws hold for ExprF") {
    import scalaz.syntax.functor._
    import scalaz.scalacheck.ScalazArbitrary._
    import scalaz.scalacheck.ScalaCheckBinding._
    import scalaz.scalacheck.ScalazProperties._
    import org.scalacheck.Arbitrary

    implicit def ExprFArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[ExprF[A]] =
      a map { a => (Plus(a, a): ExprF[A]) }

    // TODO figure out how to integrate these checks better with ScalaTest
    //      so that the test fails if one of the checks does
    equal.laws[ExprF[Int]].check
    functor.laws[ExprF].check
  }
}