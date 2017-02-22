package edu.luc.cs.cs372.expressionsAlgebraic

import org.scalacheck.{ Prop, Properties }

object behaviorTests extends Properties("behaviorTests") {

  import matryoshka.implicits._
  import behaviors._

  property("evaluate1") = Prop { (fixtures.complex1 cata evaluate) == -1 }
  property("evaluate2") = Prop { (fixtures.complex2 cata evaluate) == 0 }

  property("size1") = Prop { (fixtures.complex1 cata size) == 9 }
  property("size2") = Prop { (fixtures.complex2 cata size) == 10 }

  property("height1") = Prop { (fixtures.complex1 cata height) == 4 }
  property("height2") = Prop { (fixtures.complex2 cata height) == 5 }

  property("evaluateNat1") = Prop { (fixtures.simple1 cata evaluateNat) == Some(3) }
  property("evaluateNat2") = Prop { (fixtures.simple2 cata evaluateNat) == Some(1) }
  property("evaluateNat3") = Prop { (fixtures.complex1 cata evaluateNat) == None }
  property("evaluateNat4") = Prop { (fixtures.complex2 cata evaluateNat) == None }
}