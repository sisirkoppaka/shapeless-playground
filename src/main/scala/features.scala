package org.sisirkoppaka.playground

import shapeless._

//All thanks to Chris Vogt :)
object FeatureShowExamples extends App {
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

  println(testInput.show)

}

trait FeatureShowWrapper {
  def show: Set[String]
}

object FeatureShowWrapper {
  implicit def featureShowWrapper[T](a: T)(implicit st: FeatureShow[T]): FeatureShowWrapper = new FeatureShowWrapper {
    def show = st.show(a)
  }
}

trait FeatureShow[T] {
  def show(t: T): Set[String]
}

object FeatureShow extends LabelledTypeClassCompanion[FeatureShow] {

  implicit def intFeatureShow: FeatureShow[Int] = new FeatureShow[Int] {
    def show(n: Int) = Set(n.toString)
  }

  implicit def doubleFeatureShow: FeatureShow[Double] = new FeatureShow[Double] {
    def show(n: Double) = Set(n.toString)
  }

  object typeClass extends LabelledTypeClass[FeatureShow] {
    def emptyProduct = new FeatureShow[HNil] {
      def show(t: HNil) = Set()
    }

    def product[F, T <: HList](name: String, sh: FeatureShow[F], st: FeatureShow[T]) = new FeatureShow[F :: T] {
      def show(ft: F :: T) = {
        val head = sh.show(ft.head)
        val tail = st.show(ft.tail)
        if (tail.isEmpty)
          head.map(s => s"$name.$s")
        else
          head.map(s => s"$name.$s") ++ tail
      }
    }

    def emptyCoproduct = new FeatureShow[CNil] {
      def show(t: CNil) = Set()
    }

    def coproduct[L, R <: Coproduct](name: String, sl: => FeatureShow[L], sr: => FeatureShow[R]) = new FeatureShow[L :+: R] {
      def show(lr: L :+: R) = lr match {
        case Inl(l) => sl.show(l).map(s => s"$name.$s")
        case Inr(r) => sr.show(r)
      }
    }

    def project[F, G](instance: => FeatureShow[G], to: F => G, from: G => F) = new FeatureShow[F] {
      def show(f: F) = instance.show(to(f))
    }
  }
}
