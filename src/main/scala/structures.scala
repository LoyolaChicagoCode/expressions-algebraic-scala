package edu.luc.cs.cs372.expressionsAlgebraic

import scalaz.{ Equal, Functor }
import scalaz.std.anyVal._
import scalaz.syntax.equal._
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
  implicit object exprFFunctor extends Functor[ExprF] {
    def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case Constant(v) => Constant(v)
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
   * scalaz typeclass `Equal` using structural equality.
   * This enables `===` and `assert_===` on `ExprF` instances.
   */
  implicit def exprFEqual[A](implicit A: Equal[A]): Equal[ExprF[A]] = Equal.equal {
    case (Constant(v), Constant(w)) => v === w
    case (UMinus(r), UMinus(t))     => r === t
    case (Plus(l, r), Plus(s, t))   => (l === s) && (r === t)
    case (Minus(l, r), Minus(s, t)) => (l === s) && (r === t)
    case (Times(l, r), Times(s, t)) => (l === s) && (r === t)
    case (Div(l, r), Div(s, t))     => (l === s) && (r === t)
    case (Mod(l, r), Mod(s, t))     => (l === s) && (r === t)
    case _                          => false
  }

  /** Least fixpoint of `ExprF` as carrier object for the initial algebra. */
  type Expr = µ[ExprF]

  /** Factory for creating Expr instances. */
  object ExprFactory {
    def constant(c: Int) = In[ExprF](Constant(c))
    def uminus(r: Expr) = In[ExprF](UMinus(r))
    def plus(l: Expr, r: Expr) = In[ExprF](Plus(l, r))
    def minus(l: Expr, r: Expr) = In[ExprF](Minus(l, r))
    def times(l: Expr, r: Expr) = In[ExprF](Times(l, r))
    def div(l: Expr, r: Expr) = In[ExprF](Div(l, r))
    def mod(l: Expr, r: Expr) = In[ExprF](Mod(l, r))
  }
}