package org.sisirkoppaka.playground

import shapeless._
import record._
import ops.record._
import syntax.singleton._

//The Convertible is a nice example from Travis Brown.
//We make a trivial extension to InterConvertible, for a bijection of Convertible between any two arbitrary case classes.
//We then make a purely typed version that needs no instances of the case class, InterConvertibleTyped.
object CaseClassConvertible extends App {
  import AllConvertibles._
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

  def abInterConvertTyped = new InterConvertibleTyped[A,B]

  println("InterConvertibleTyped for A and B without instances is " + abInterConvertTyped() )

}

object AllConvertibles {
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

  //Think about it, InterConvertible shouldn't really need an instance to say if two case classes are InterConvertible
  //All you need are the case class definitions (and the field name and field types)
  class InterConvertibleTyped[T, S] {
    def apply[SH <: HList, TH <: HList, SHK <: HList, THK <: HList]()(implicit
      genS: LabelledGeneric.Aux[S, SH],
      genT: LabelledGeneric.Aux[T, TH],
      genSKeys: Keys.Aux[SH, SHK],
      genTKeys: Keys.Aux[TH, THK],
      alignST: ops.hlist.Align[SHK, THK],
      alignTS: ops.hlist.Align[THK, SHK]
    ) = (alignST(genSKeys()) == genTKeys()) && (alignTS(genTKeys()) == genSKeys())
  }
}
