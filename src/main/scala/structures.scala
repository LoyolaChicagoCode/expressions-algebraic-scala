package edu.luc.cs.cs371.expressionsAlgebraic

import cats.{Applicative, Eq, Eval, Functor, Show, Traverse}
import higherkindness.droste.data.Fix

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
    case UMinus[A](expr: A) extends ExprF[A]
    case Plus[A](left: A, right: A) extends ExprF[A]
    case Minus[A](left: A, right: A) extends ExprF[A]
    case Times[A](left: A, right: A) extends ExprF[A]
    case Div[A](left: A, right: A) extends ExprF[A]
    case Mod[A](left: A, right: A) extends ExprF[A]

  import ExprF.*

  /**
    * Implicit value for declaring `ExprF` as an instance of
    * typeclass `Functor` in Cats. This requires us to define
    * `map`.
    * Not really needed in the presence of the `Traverse`
    * instance below, which defines a further generalization
    * of `map`.
    */
  object exprFFunctor extends Functor[ExprF]:
    override def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match
      case c @ Constant(v) => c
      case UMinus(r)       => UMinus(f(r))
      case Plus(l, r)      => Plus(f(l), f(r))
      case Minus(l, r)     => Minus(f(l), f(r))
      case Times(l, r)     => Times(f(l), f(r))
      case Div(l, r)       => Div(f(l), f(r))
      case Mod(l, r)       => Mod(f(l), f(r))
  end exprFFunctor

  given [T](using Eq[T]): Eq[ExprF[T]] = Eq.fromUniversalEquals

  given [T](using Show[T]): Show[ExprF[T]] = Show.fromToString

  // TODO bring this back

  /**
    * Implicit value for declaring `ExprF` as an instance of
    * typeclass `Traverse` in Cats. This requires us to define
    * `traverse`.
    */
  given Traverse[ExprF] = new Traverse[ExprF]:
    import cats.implicits.*
    override def traverse[G[_]: Applicative, A, B](fa: ExprF[A])(f: A => G[B]): G[ExprF[B]] = fa match
      case c @ Constant(v) => c.pure[G]
      case UMinus(r)       => f(r).map(UMinus(_))
      case Plus(l, r)      => (f(l), f(r)).mapN(Plus(_, _))
      case Minus(l, r)     => (f(l), f(r)).mapN(Minus(_, _))
      case Times(l, r)     => (f(l), f(r)).mapN(Times(_, _))
      case Div(l, r)       => (f(l), f(r)).mapN(Div(_, _))
      case Mod(l, r)       => (f(l), f(r)).mapN(Mod(_, _))
    // TODO working implementations of foldRight and foldLeft
    // problem: need Monoid or Applicative to implement foldRight as foldMap or traverse
    def foldRight[A, B](fa: ExprF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
    def foldLeft[A, B](fa: ExprF[A], b: B)(f: (B, A) => B): B = ???
    //    override def foldMap[A, B](fa: ExprF[A])(f: A => B)(implicit B: Monoid[B]): B =
    //      traverse[Const[B, *], A, B](fa)(a => Const(f(a))).getConst
    //      foldMap(fa)(f(_, lb))
    //  import cats.data.Const
    //      traverse[Const[Eval[B], *], A, Eval[B]](fa)(a => Const(f(a, lb))).getConst

  /** Least fixpoint of `ExprF` as carrier object for the initial algebra. */
  type Expr = Fix[ExprF]

  /** Enable typesafe equality for `Expr`. */
  given CanEqual[Expr, Expr] = CanEqual.derived

  /** Factory for creating Expr instances. */
  object Expr:
    def constant(c: Int) = Fix(Constant(c))
    def uminus(r: Expr) = Fix(UMinus(r))
    def plus(l: Expr, r: Expr) = Fix(Plus(l, r))
    def minus(l: Expr, r: Expr) = Fix(Minus(l, r))
    def times(l: Expr, r: Expr) = Fix(Times(l, r))
    def div(l: Expr, r: Expr) = Fix(Div(l, r))
    def mod(l: Expr, r: Expr) = Fix(Mod(l, r))
  end Expr

  given Eq[Expr] = Eq.fromUniversalEquals

  given Show[Expr] = Show.fromToString

end structures
