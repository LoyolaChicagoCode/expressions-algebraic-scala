package edu.luc.cs.cs371.expressionsAlgebraic

import cats.implicits._
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}

object lawTests extends Properties("lawTests") {

  import structures._, ExprF._, Expr._

  property("equals1") = Prop { (Constant(3): ExprF[Nothing]) === Constant(3) }
  property("equals2") = Prop { constant(3) === constant(3) }
  property("equals3") = Prop { uminus(constant(3)) === uminus(constant(3)) }

  property("toString1") = Prop { UMinus(Constant(3)).toString === "UMinus(Constant(3))" }
  property("toString2") = Prop { uminus(constant(3)).toString === "UMinus(Constant(3))" }

  // TODO check if Traverse can help with this
  // η = point, ∘ = map, ⊛ = apply2

  def genConstant(g: Gen[Int]) = g.map(Constant(_))
  def genUMinus[A](g: Gen[A]) = g.map(UMinus(_))
  def genPlus[A](g: Gen[A]) = (g, g).mapN(Plus(_, _))
  def genMinus[A](g: Gen[A]) = (g, g).mapN(Minus(_, _))
  def genTimes[A](g: Gen[A]) = (g, g).mapN(Times(_, _))
  def genDiv[A](g: Gen[A]) = (g, g).mapN(Div(_, _))
  def genMod[A](g: Gen[A]) = (g, g).mapN(Mod(_, _))

  implicit def exprFArbitrary[A: Arbitrary]: Arbitrary[ExprF[A]] = Arbitrary {
    val i = Arbitrary.arbInt.arbitrary
    val g = Arbitrary.arbitrary[A]
    Gen.oneOf(genConstant(i), genUMinus(g), genPlus(g), genMinus(g), genTimes(g), genDiv(g), genMod(g))
  }

  //  include(equal.laws[ExprF[Int]], "equalExprFInt.")
  //  include(equal.laws[ExprF[ExprF[Int]]], "equalExprF2Int.")
  //  include(equal.laws[ExprF[ExprF[ExprF[Int]]]], "equalExprF3Int.")

  // FIXME https://github.com/LoyolaChicagoCode/expressions-algebraic-scala/issues/15
  //include(equal.laws[Expr], "equalExpr.")

  include(cats.laws.discipline.FunctorTests[ExprF].functor[Int, Int, Int].all)
  //  include(cats.laws.discipline.TraverseTests[ExprF].traverse[Int, Int, Int, Int, Option, Option].all)
}
