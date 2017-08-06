package io.scalac.ft

import io.scalac.ft.Basic.{Language, NoWrap}

object Extended extends App {
  trait LanguageWithMul[Wrapper[_]] extends Language[Wrapper] {
    def multiply(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]
  }
  trait ScalaToLanguageWithMulBridge[ScalaValue] {
    def apply[Wrapper[_]](implicit L: LanguageWithMul[Wrapper]): Wrapper[ScalaValue]
  }
  def multiply(a: Int, b: Int) = new ScalaToLanguageWithMulBridge[Int] {
    override def apply[Wrapper[_]](implicit L: LanguageWithMul[Wrapper]): Wrapper[Int] = {
      L.multiply(L.number(a), L.number(b))
    }
  }
  val multiplyExpression = multiply(2,3)
  val interpretWithMul = new LanguageWithMul[NoWrap] {
    override def multiply(a: NoWrap[Int], b: NoWrap[Int]): NoWrap[Int] = a * b

    override def number(v: Int): NoWrap[Int] = v
    override def increment(a: NoWrap[Int]): NoWrap[Int] = a + 1
    override def add(a: NoWrap[Int], b: NoWrap[Int]): NoWrap[Int] = a + b

    override def text(v: String): NoWrap[String] = v
    override def toUpper(a: NoWrap[String]): NoWrap[String] = a.toUpperCase
    override def concat(a: NoWrap[String], b: NoWrap[String]): NoWrap[String] = a + " " + b

    override def toString(v: NoWrap[Int]): NoWrap[String] = v.toString
  }

  println(s"interpreted multiply: ${multiplyExpression.apply(interpretWithMul)}")
}