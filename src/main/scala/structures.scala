package edu.luc.cs.cs371.expressionsAlgebraic

import cats.{Applicative, Eq, Show, Traverse}
import higherkindness.droste.data.Fix
import higherkindness.droste.util.DefaultTraverse

/**
  * In this example, we represent arithmetic expressions as trees
  * (initial algebra for the endofunctor defined below).
  */
object structures:

  /**
    * Endofunctor for (nongeneric) F-algebra in the category Scala types.
    * Note that `A` is not a generic item type of the resulting algebraic
    * data type. As can be seen below, once we form `Expr` as the least
    * fixpoint of `ExprF`, `A` will go away.
    *
    * @tparam A argument of the endofunctor
    */
  enum ExprF[+A] derives CanEqual:
    case Constant(value: Int) extends ExprF[Nothing]
    case UMinus(expr: A)
    case Plus(left: A, right: A)
    case Minus(left: A, right: A)
    case Times(left: A, right: A)
    case Div(left: A, right: A)
    case Mod(left: A, right: A)

  import ExprF.*

  /**
    * Implicit value for declaring `ExprF` as an instance of
    * typeclass `Functor` in Cats. This requires us to define
    * `map`.
    * Not really needed in the presence of the `Traverse`
    * instance below, which defines a further generalization
    * of `map`.
    */
//  given Functor[ExprF] = new Functor[ExprF]:
//    override def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match
//      case c @ Constant(v) => c
//      case UMinus(r)       => UMinus(f(r))
//      case Plus(l, r)      => Plus(f(l), f(r))
//      case Minus(l, r)     => Minus(f(l), f(r))
//      case Times(l, r)     => Times(f(l), f(r))
//      case Div(l, r)       => Div(f(l), f(r))
//      case Mod(l, r)       => Mod(f(l), f(r))
//  // end Functor[ExprF]

  given [T](using Eq[T]): Eq[ExprF[T]] = Eq.fromUniversalEquals

  given [T](using Show[T]): Show[ExprF[T]] = Show.fromToString

  /**
    * Implicit value for declaring `ExprF` as an instance of
    * typeclass `Traverse` in Cats. This requires us to define
    * `traverse`.
    */
  given Traverse[ExprF] = new DefaultTraverse[ExprF]:
    import cats.syntax.all.*
    override def traverse[G[_]: Applicative, A, B](fa: ExprF[A])(f: A => G[B]): G[ExprF[B]] = fa match
      case c @ Constant(v) => c.pure[G]
      case UMinus(r)       => f(r).map(UMinus(_))
      case Plus(l, r)      => (f(l), f(r)).mapN(Plus(_, _))
      case Minus(l, r)     => (f(l), f(r)).mapN(Minus(_, _))
      case Times(l, r)     => (f(l), f(r)).mapN(Times(_, _))
      case Div(l, r)       => (f(l), f(r)).mapN(Div(_, _))
      case Mod(l, r)       => (f(l), f(r)).mapN(Mod(_, _))
  // end Traverse[ExprF]

  /** Least fixpoint of `ExprF` as carrier object for the initial algebra. */
  type Expr = Fix[ExprF]

  /** Enable typesafe equality for `Expr`. */
  given CanEqual[Expr, Expr] = CanEqual.derived

  /** Factory for creating Expr instances. */
  object Expr:
    def constant(c: Int): Expr = Fix(Constant(c))
    def uminus(r: Expr): Expr = Fix(UMinus(r))
    def plus(l: Expr, r: Expr): Expr = Fix(Plus(l, r))
    def minus(l: Expr, r: Expr): Expr = Fix(Minus(l, r))
    def times(l: Expr, r: Expr): Expr = Fix(Times(l, r))
    def div(l: Expr, r: Expr): Expr = Fix(Div(l, r))
    def mod(l: Expr, r: Expr): Expr = Fix(Mod(l, r))
  end Expr

  given Eq[Expr] = Eq.fromUniversalEquals

  given Show[Expr] = Show.fromToString

end structures
