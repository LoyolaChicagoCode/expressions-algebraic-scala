package edu.luc.cs.cs372.expressionsAlgebraic

import matryoshka.Algebra

/**
 * In this example, we define several behaviors of arithmetic expressions
 * as specific ExprF-algebras. Note the nonrecursive nature of these
 * algebras.
 */
object behaviors {

  import structures._

  // TODO parsing as unfold (prefix or postfix notation)
  // TODO unparsing/printing

  /** Evaluates an expression to a value. */
  val evaluate: Algebra[ExprF, Int] = {
    case Constant(c) => c
    case UMinus(r)   => -r
    case Plus(l, r)  => l + r
    case Minus(l, r) => l - r
    case Times(l, r) => l * r
    case Div(l, r)   => l / r
    case Mod(l, r)   => l % r
  }

  /** Computes the number of nodes in an expression. */
  val size: Algebra[ExprF, Int] = {
    case Constant(_) => 1
    case UMinus(r)   => 1 + r
    case Plus(l, r)  => 1 + l + r
    case Minus(l, r) => 1 + l + r
    case Times(l, r) => 1 + l + r
    case Div(l, r)   => 1 + l + r
    case Mod(l, r)   => 1 + l + r
  }

  /** Computes the height of an expression tree. */
  val height: Algebra[ExprF, Int] = {
    case Constant(_) => 1
    case UMinus(r)   => 1 + r
    case Plus(l, r)  => 1 + math.max(l, r)
    case Minus(l, r) => 1 + math.max(l, r)
    case Times(l, r) => 1 + math.max(l, r)
    case Div(l, r)   => 1 + math.max(l, r)
    case Mod(l, r)   => 1 + math.max(l, r)
  }

  /**
   * Evaluates an expression representing a natural number.
   * If any of the partial results becomes negative,
   * evaluation fails.
   */
  val evaluateNat: Algebra[ExprF, Option[Int]] = {
    case Constant(c) => for { v <- Some(c); if v >= 0 } yield v
    case UMinus(r)   => None
    case Plus(l, r)  => for { l1 <- l; r1 <- r } yield l1 + r1
    case Minus(l, r) => for { l1 <- l; r1 <- r; if l1 >= r1 } yield l1 - r1
    case Times(l, r) => for { l1 <- l; r1 <- r } yield l1 * r1
    case Div(l, r)   => for { l1 <- l; r1 <- r } yield l1 / r1
    case Mod(l, r)   => for { l1 <- l; r1 <- r } yield l1 % r1
  }
}