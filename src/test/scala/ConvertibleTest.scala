package org.sisirkoppaka.playground

import org.scalatest.FunSuite

class ConvertibleTest extends FunSuite {
  test("should work for all basic cases") {
    import AllConvertibles._

    def convertToT[T] = new Convertible[T]
    def convertToS[S] = new Convertible[S]
    case class A(foo: Int, bar: String)
    case class B(bar: String, foo: Int)

    val a: A = A(12, "foo")
    val b: B = convertToT[B](a)
    val aBack: A = convertToS[A](b)

    assert( a == aBack )

    def abInterConvert = new InterConvertible[A,B]

    assert( abInterConvert(a,b) )

    def abInterConvertTyped = new InterConvertibleTyped[A,B]

    assert( abInterConvertTyped() )
  }
}
