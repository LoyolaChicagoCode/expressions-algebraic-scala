package edu.luc.cs.cs372.expressionsAlgebraic

import scalaz.Functor
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
   * typeclass `Functor` in scalaz.
   */
  implicit object ExprFunctor extends Functor[ExprF] {
    def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case Constant(v) => Constant(v)
      case UMinus(r)   => UMinus(f(r))
      case Plus(l, r)  => Plus (f(l), f(r))
      case Minus(l, r) => Minus(f(l), f(r))
      case Times(l, r) => Times(f(l), f(r))
      case Div(l, r)   => Div (f(l), f(r))
      case Mod(l, r)   => Mod (f(l), f(r))
    }
  }

  /**
   * Least fixpoint of `ExprF` as carrier object for the initial algebra.
   */
  type Expr = µ[ExprF]

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