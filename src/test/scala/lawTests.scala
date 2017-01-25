package edu.luc.cs.cs372.expressionsAlgebraic

import scalaz.{ Equal, Functor }
import scalaz.syntax.functor._
import scalaz.syntax.apply._
import scalaz.std.anyVal._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.{ Arbitrary, Gen, Prop, Properties }
import Gen._
import Arbitrary._

object lawTests extends Properties("lawTests") {

  import structures._
  import structures.Expr._

  property("equals1") = Prop { Constant(3) == Constant(3) }
  property("equals2") = Prop { constant(3) == constant(3) }
  property("equals3") = Prop { uminus(constant(3)) == uminus(constant(3)) }

  property("toString1") = Prop { UMinus(Constant(3)).toString == "UMinus(Constant(3))" }
  property("toString1") = Prop { uminus(constant(3)).toString == "Fix(UMinus(Fix(Constant(3))))" }

  // TODO check if Traverse can help with this

  def genConstant[A](g: Gen[Int]) = g.map(Constant(_))
  def genUMinus[A](g: Gen[A]) = g.map(UMinus(_))
  def genPlus[A](g: Gen[A]) = (g ⊛ g)(Plus(_, _))
  def genMinus[A](g: Gen[A]) = (g ⊛ g)(Minus(_, _))
  def genTimes[A](g: Gen[A]) = (g ⊛ g)(Times(_, _))
  def genDiv[A](g: Gen[A]) = (g ⊛ g)(Div(_, _))
  def genMod[A](g: Gen[A]) = (g ⊛ g)(Mod(_, _))

  // required for non-Delay implicits to arise automatically from Delay ones
  import matryoshka._
  import matryoshka.implicits._
  import matryoshka.scalacheck.arbitrary._

  implicit object exprFArbitraryD extends Delay[Arbitrary, ExprF] {
    def apply[A](a: Arbitrary[A]) = Arbitrary {
      val i = arbInt.arbitrary
      val g = a.arbitrary
      oneOf(genConstant(i), genUMinus(g), genPlus(g), genMinus(g), genTimes(g), genDiv(g), genMod(g))
    }
  }

  property("equalsExprFInt") = equal.laws[ExprF[Int]]
  property("equalsExprFExprFInt") = equal.laws[ExprF[ExprF[Int]]]
  property("equalsExprFExprFExprFInt") = equal.laws[ExprF[ExprF[ExprF[Int]]]]
  // FIXME https://github.com/LoyolaChicagoCode/expressions-algebraic-scala/issues/15
  // property("equalsExpr") = equal.laws[Expr]

  property("functorExprF") = functor.laws[ExprF]
  property("traverseExprF") = traverse.laws[ExprF]
}