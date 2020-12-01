package edu.luc.cs.cs371.expressionsAlgebraic

import cats.implicits._
import higherkindness.droste._
import org.scalacheck.{Prop, Properties}

object behaviorTests extends Properties("behaviorTests") {

  import behaviors._

  val ev = scheme.cata(evaluate)
  property("evaluate1") = Prop { ev(fixtures.complex1) === -1 }
  property("evaluate2") = Prop { ev(fixtures.complex2) === 0 }

  val si = scheme.cata(size)
  property("size1") = Prop { si(fixtures.complex1) === 9 }
  property("size2") = Prop { si(fixtures.complex2) === 10 }

  val he = scheme.cata(height)
  property("height1") = Prop { he(fixtures.complex1) === 4 }
  property("height2") = Prop { he(fixtures.complex2) === 5 }

  val en = scheme.cata(evaluateNat)
  property("evaluateNat1") = Prop { en(fixtures.simple1) === Some(3) }
  property("evaluateNat2") = Prop { en(fixtures.simple2) === Some(1) }
  property("evaluateNat3") = Prop { en(fixtures.complex1) === None }
  property("evaluateNat4") = Prop { en(fixtures.complex2) === None }

  // Equality is defined only on the common endofunctor supertype ExprF.
  // Therefore, we need to use a type annotation to enable equality on
  // a specific variant of the endofunctor.
  import structures._
  val co = UMinus(Constant(1))
  property("safeEq") = Prop { (co: ExprF[Constant]) === co }
  property("unsafeEq") = Prop { co == co }
}
