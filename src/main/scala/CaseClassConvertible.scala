package org.sisirkoppaka.playground

import shapeless._

//The Convertible is a nice example from Travis Brown.
//A trivial extension to InterConvertible, for a bijection of Convertible between any two arbitrary case classes.
object CaseClassConvertible extends App {
  def convertToT[T] = new Convertible[T]
  def convertToS[S] = new Convertible[S]
  case class A(foo: Int, bar: String)
  case class B(bar: String, foo: Int)
  val a: A = A(12, "foo")
  val b: B = convertToT[B](a)
  val aBack: A = convertToS[A](b)

  def abInterConvert = new InterConvertible[A,B]

  println("a: " + a + " => b: " + b)
  println("aBack: " + aBack)
  println("interconvertible: " + abInterConvert(a,b) )

}

//From Travis
class Convertible[T] {
  def apply[S, SH <: HList, TH <: HList](s: S)(implicit
    genS: LabelledGeneric.Aux[S, SH],
    genT: LabelledGeneric.Aux[T, TH],
    align: ops.hlist.Align[SH, TH]
  ) = genT.from(align(genS.to(s)))
}

//Extensions of the idea, a bijection of the convertible trait, for any two arbitrary case classes
class InterConvertible[T, S] {
  def apply[SH <: HList, TH <: HList](t: T, s: S)(implicit
    genS: LabelledGeneric.Aux[S, SH],
    genT: LabelledGeneric.Aux[T, TH],
    alignST: ops.hlist.Align[SH, TH],
    alignTS: ops.hlist.Align[TH, SH]
  ) =
    (genT.from(alignST(genS.to(s))) == t) && (genS.from(alignTS(genT.to(t))) == s)
}
