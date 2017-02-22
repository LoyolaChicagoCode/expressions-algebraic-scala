package edu.luc.cs.cs372.expressionsAlgebraic

import scalaz.{ Applicative, Equal, Functor, Show, Traverse }
import matryoshka.Delay
import matryoshka.data.Fix

/**
 * In this example, we represent arithmetic expressions as trees
 * (initial algebra for the endofunctor defined below).
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
  case class Constant[A](value: Int) extends ExprF[A]
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
   * Not really needed in the presence of the `Traverse`
   * instance below, which defines a further generalization
   * of `map`.
   */
  object exprFFunctor extends Functor[ExprF] {
    def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case Constant(v) => Constant[B](v)
      case UMinus(r)   => UMinus(f(r))
      case Plus(l, r)  => Plus(f(l), f(r))
      case Minus(l, r) => Minus(f(l), f(r))
      case Times(l, r) => Times(f(l), f(r))
      case Div(l, r)   => Div(f(l), f(r))
      case Mod(l, r)   => Mod(f(l), f(r))
    }
  }

  /**
   * Implicit value for declaring `ExprF` as an instance of
   * typeclass `Traverse` in scalaz. This requires us to define
   * `traverseImpl`.
   */
  implicit object exprFTraverse extends Traverse[ExprF] {
    import scalaz.syntax.applicative._ // η = point, ∘ = map, ⊛ = apply2
    def traverseImpl[G[_], A, B](fa: ExprF[A])(f: A => G[B])(implicit a: Applicative[G]): G[ExprF[B]] = fa match {
      case Constant(v) => (Constant(v): ExprF[B]).η
      case UMinus(r)   => f(r) ∘ (UMinus(_))
      case Plus(l, r)  => (f(l) ⊛ f(r))(Plus(_, _))
      case Minus(l, r) => (f(l) ⊛ f(r))(Minus(_, _))
      case Times(l, r) => (f(l) ⊛ f(r))(Times(_, _))
      case Div(l, r)   => (f(l) ⊛ f(r))(Div(_, _))
      case Mod(l, r)   => (f(l) ⊛ f(r))(Mod(_, _))
    }
  }

  /** Least fixpoint of `ExprF` as carrier object for the initial algebra. */
  type Expr = Fix[ExprF]

  /** Factory for creating Expr instances. */
  object Expr {
    def constant(c: Int) = Fix[ExprF](Constant(c))
    def uminus(r: Expr) = Fix[ExprF](UMinus(r))
    def plus(l: Expr, r: Expr) = Fix[ExprF](Plus(l, r))
    def minus(l: Expr, r: Expr) = Fix[ExprF](Minus(l, r))
    def times(l: Expr, r: Expr) = Fix[ExprF](Times(l, r))
    def div(l: Expr, r: Expr) = Fix[ExprF](Div(l, r))
    def mod(l: Expr, r: Expr) = Fix[ExprF](Mod(l, r))
  }

  /** Also gives rise to non-delayed `Equal` instances for `ExprF` and `Expr`. */
  implicit object exprFEqualD extends Delay[Equal, ExprF] {
    def apply[A](a: Equal[A]) = Equal.equalA[ExprF[A]]
  }

  /** Also gives rise to non-delayed `Show` instances for `ExprF` and `Expr`. */
  implicit object exprFShowD extends Delay[Show, ExprF] {
    def apply[A](a: Show[A]) = Show.showFromToString[ExprF[A]]
  }
}