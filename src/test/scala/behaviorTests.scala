package edu.luc.cs.cs372.expressionsAlgebraic

import org.scalatest.FunSuite
import scalaz.syntax.equal._
import scalaz.std.anyVal._ // for assert_=== to work on Int
import scalaz.std.option._ // for assert_=== to work on Option

class behaviorTests extends FunSuite {

  import scalamu.ToMuOps
  import behaviors._

  test("evaluate works") {
    fixtures.complex1 cata evaluate assert_=== -1
    fixtures.complex2 cata evaluate assert_=== 0
  }

  test("size works") {
    fixtures.complex1 cata size assert_=== 9
    fixtures.complex2 cata size assert_=== 10
  }

  test("height works") {
    fixtures.complex1 cata height assert_=== 4
    fixtures.complex2 cata height assert_=== 5
  }

  test("evaluateNat works") {
    fixtures.simple1 cata evaluateNat assert_=== Some(3)
    fixtures.simple2 cata evaluateNat assert_=== Some(1)
    fixtures.complex1 cata evaluateNat assert_=== None
    fixtures.complex2 cata evaluateNat assert_=== None
  }
}