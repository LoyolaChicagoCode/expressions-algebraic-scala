package edu.luc.cs.cs372.expressionsAlgebraic

import scalaz.{ Equal, Functor, Show }
import scalamu._ // algebra types and injected cata method

/*
 * In this example, we represent arithmetic expressions as trees
 * (initial algebra for the endofunctor defined next).
 */
object structures {

  /**
   * Endofunctor for (nongeneric) F-algebra in the category Scala types.
   * Note that `A` is not a generic item type of the resulting algebraic
   * data type. As can be seen below, once we form `Expr` as the least
   * fixpoint of `ExprF`, `A` will go away.
   *
   * @tparam A argument of the endofunctor
   */
  sealed trait ExprF[+A]
  case class Constant(value: Int) extends ExprF[Nothing]
  case class UMinus[A](expr: A) extends ExprF[A]
  case class Plus[A](left: A, right: A) extends ExprF[A]
  case class Minus[A](left: A, right: A) extends ExprF[A]
  case class Times[A](left: A, right: A) extends ExprF[A]
  case class Div[A](left: A, right: A) extends ExprF[A]
  case class Mod[A](left: A, right: A) extends ExprF[A]

  /**
   * Implicit value for declaring `ExprF` as an instance of
   * typeclass `Functor` in scalaz. This requires us to define
   * `map`.
   */
  implicit object ExprFFunctor extends Functor[ExprF] {
    def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case Constant(v) => Constant(v)
      case UMinus(r)   => UMinus(f(r))
      case Plus(l, r)  => Plus(f(l), f(r))
      case Minus(l, r) => Minus(f(l), f(r))
      case Times(l, r) => Times(f(l), f(r))
      case Div(l, r)   => Div (f(l), f(r))
      case Mod(l, r)   => Mod (f(l), f(r))
    }
  }

  /**
   * Implicit value for declaring `ExprF` as an instance of
   * typeclass `Equal` in scalaz using structural equality.
   * This enables `===` and `assert_===` on `ExprF` instances.
   */
  private trait ExprFEqual[A] extends Equal[ExprF[A]] {
    implicit def A: Equal[A]
    override def equalIsNatural: Boolean = A.equalIsNatural
    override def equal(a1: ExprF[A], a2: ExprF[A]) = (a1, a2) match {
      case (Constant(v), Constant(w)) => v == w
      case (UMinus(r), UMinus(t))     => A.equal(r, t)
      case (Plus(l, r), Plus(s, t))   => A.equal(l, s) && A.equal(r, t)
      case (Minus(l, r), Minus(s, t)) => A.equal(l, s) && A.equal(r, t)
      case (Times(l, r), Times(s, t)) => A.equal(l, s) && A.equal(r, t)
      case (Div(l, r), Div(s, t))     => A.equal(l, s) && A.equal(r, t)
      case (Mod(l, r), Mod(s, t))     => A.equal(l, s) && A.equal(r, t)
      case _ => false
    }
  }
  implicit def exprFEqual[A](implicit A0: Equal[A]): Equal[ExprF[A]] = new ExprFEqual[A] {
    implicit def A = A0
  }

  /**
   * Implicit value for declaring `ExprF` as an instance of
   * typeclass `Show` in scalaz using `Show`'s default method.
   * This is required for `===` and `assert_===` to work on `ExprF` instances.
   */
  implicit def exprFShow[A](implicit A: Show[A]): Show[ExprF[A]] = new Show[ExprF[A]] {
    override def show(e: ExprF[A]): scalaz.Cord = e match {
      case Constant(v) => "Constant(" ++ v.toString ++ ")"
      case UMinus(r)   => "UMinus(" +: A.show(r) :+ ")"
      case Plus(l, r)  => ("Plus("  +: A.show(l) :+ ",") ++ A.show(r) :+ ")"
      case Minus(l, r) => ("Minus(" +: A.show(l) :+ ",") ++ A.show(r) :+ ")"
      case Times(l, r) => ("Times(" +: A.show(l) :+ ",") ++ A.show(r) :+ ")"
      case Div(l, r)   => ("Div("   +: A.show(l) :+ ",") ++ A.show(r) :+ ")"
      case Mod(l, r)   => ("Mod("   +: A.show(l) :+ ",") ++ A.show(r) :+ ")"
    }
  }

  /** Least fixpoint of `ExprF` as carrier object for the initial algebra. */
  type Expr = µ[ExprF]

  /** Factory for creating Expr instances. */
  object ExprFactory {
    def constant(c: Int): Expr = In(Constant(c))
    def uminus(r: Expr): Expr = In(UMinus(r))
    def plus(l: Expr, r: Expr): Expr = In(Plus (l, r))
    def minus(l: Expr, r: Expr): Expr = In(Minus(l, r))
    def times(l: Expr, r: Expr): Expr = In(Times(l, r))
    def div(l: Expr, r: Expr): Expr = In(Div (l, r))
    def mod(l: Expr, r: Expr): Expr = In(Mod (l, r))
  }
}