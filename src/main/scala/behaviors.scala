package edu.luc.cs.cs372.expressionsAlgebraic

object behaviors {

  import scalamu.Algebra
  import structures._

  // TODO parsing as unfold (prefix or postfix notation)
  // TODO unparsing/printing

  // specific ExprF-algebras: note nonrecursive nature

  def evaluate: Algebra[ExprF, Int] = {
    case Constant(c) => c
    case UMinus(r)   => -r
    case Plus(l, r)  => l + r
    case Minus(l, r) => l - r
    case Times(l, r) => l * r
    case Div(l, r)   => l / r
    case Mod(l, r)   => l % r
  }

  def size: Algebra[ExprF, Int] = {
    case Constant(_) => 1
    case UMinus(r)   => 1 + r
    case Plus(l, r)  => 1 + l + r
    case Minus(l, r) => 1 + l + r
    case Times(l, r) => 1 + l + r
    case Div(l, r)   => 1 + l + r
    case Mod(l, r)   => 1 + l + r
  }

  def depth: Algebra[ExprF, Int] = {
    case Constant(_) => 1
    case UMinus(r)   => 1 + r
    case Plus(l, r)  => 1 + math.max(l, r)
    case Minus(l, r) => 1 + math.max(l, r)
    case Times(l, r) => 1 + math.max(l, r)
    case Div(l, r)   => 1 + math.max(l, r)
    case Mod(l, r)   => 1 + math.max(l, r)
  }
}