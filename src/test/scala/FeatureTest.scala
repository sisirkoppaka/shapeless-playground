package org.sisirkoppaka.playground

import org.scalatest.FunSuite

class FeatureTest extends FunSuite {
  test("should work for a naive feature hierarchy") {
    import FeatureShowWrapper._

    sealed trait feature
    case class A(i: Int, b: B) extends feature
    case class B(c: C, d: D) extends feature
    case class C(x: Int, z: Int) extends feature
    case class D(y: Int) extends feature

    val testInput: feature = A(i = 5, b = B(c = C(x = 6, z = 9), d = D(y = 5)))

    object feature {
      implicit val instance =  FeatureShow[feature]
    }

    assert(testInput.show === Set("A.i.5", "A.b.c.x.6", "A.b.c.z.9", "A.b.d.y.5") )
  }

  test("should work for a more intertwined feature hierarchy") {
    import FeatureShowWrapper._

    sealed trait Feature

    sealed trait FeatureDate extends Feature
    case class ADate(i: Int, b: BDate) extends FeatureDate
    case class BDate(c: CTime, d: DDate) extends FeatureDate
    case class CDate(x: Int, z: Int) extends FeatureDate
    case class DDate(y: Int) extends FeatureDate

    sealed trait FeatureTime extends Feature
    case class ATime(i: Int, b: BTime) extends FeatureTime
    case class BTime(c: CDate, d: DTime) extends FeatureTime
    case class CTime(x: Int, z: Int) extends FeatureTime
    case class DTime(y: Int) extends FeatureTime

    val testInput: Feature = ADate(i = 5, b = BDate(c = CTime(x = 6, z = 9), d = DDate(y = 5)))

    object feature {
      implicit val instance =  FeatureShow[Feature]
    }

    assert(testInput.show === Set("ADate.i.5", "ADate.b.c.x.6", "ADate.b.c.z.9", "ADate.b.d.y.5") )
  }
}
