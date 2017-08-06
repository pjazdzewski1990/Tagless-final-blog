package io.scalac.ft

import io.scalac.ft.Basic.{Language, ScalaToLanguageBridge}

object Optimized extends App {

  // builds a 10 + (((0 + 1)+1)+1) expression
  def buildIncrementExpression() = new ScalaToLanguageBridge[Int] {
    override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = {
      L.add(L.number(10), L.increment(L.increment(L.increment(L.number(0)))))
    }
  }

  // as seen in earlier example
  type NoWrap[ScalaValue] = ScalaValue
  val interpret = new Language[NoWrap] {
    override def number(v: Int): NoWrap[Int] = v
    override def increment(a: NoWrap[Int]): NoWrap[Int] = a + 1
    override def add(a: NoWrap[Int], b: NoWrap[Int]): NoWrap[Int] = a + b

    override def text(v: String): NoWrap[String] = v
    override def toUpper(a: NoWrap[String]): NoWrap[String] = a.toUpperCase
    override def concat(a: NoWrap[String], b: NoWrap[String]): NoWrap[String] = a + " " + b

    override def toString(v: NoWrap[Int]): NoWrap[String] = v.toString
  }
  type PrettyPrint[ScalaValue] = String
  val interpretAsPrettyPrint = new Language[PrettyPrint] {
    override def number(v: Int): PrettyPrint[Int] = s"($v)"
    override def increment(a: PrettyPrint[Int]): PrettyPrint[Int] = s"(inc $a)"
    override def add(a: PrettyPrint[Int], b: PrettyPrint[Int]): PrettyPrint[Int] = s"(+ $a $b)"

    override def text(v: String): PrettyPrint[String] = s"[$v]"
    override def toUpper(a: PrettyPrint[String]): PrettyPrint[String] = s"(toUpper $a)"
    override def concat(a: PrettyPrint[String], b: PrettyPrint[String]): PrettyPrint[String] = s"(concat $a $b)"

    override def toString(v: PrettyPrint[Int]): PrettyPrint[String] = s"(toString $v)"
  }

  val simpleVersion = buildIncrementExpression()
  println(s"Unoptimized ${simpleVersion.apply(interpretAsPrettyPrint)} = ${simpleVersion.apply(interpret)}")

  type Nested[ScalaValue] = ScalaToLanguageBridge[ScalaValue]
  val simplify = new Language[Nested]{
    var nesting = 0

    override def number(v: Int): Nested[Int] = new ScalaToLanguageBridge[Int] {
      override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = {
        if(nesting > 0) {
          val temp = nesting
          nesting = 0
          L.add(L.number(temp), L.number(v))
        } else {
          L.number(v)
        }
      }
    }
    override def increment(a: ScalaToLanguageBridge[Int]): Nested[Int] = new ScalaToLanguageBridge[Int] {
      override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = {
        nesting = nesting + 1
        a.apply(L)
      }
    }
    override def add(a: ScalaToLanguageBridge[Int], b: ScalaToLanguageBridge[Int]): Nested[Int] = new ScalaToLanguageBridge[Int] {
      override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = {
        if(nesting > 0){
          val temp = nesting
          nesting = 0
          L.add(L.number(temp), L.add(a.apply(L), b.apply(L)))
        } else {
          L.add(a.apply(L), b.apply(L))
        }
      }
    }

    override def text(v: String): Nested[String] = ???
    override def toUpper(a: Nested[String]): Nested[String] = ???
    override def concat(a: Nested[String], b: Nested[String]): Nested[String] = ???

    override def toString(v: Nested[Int]): Nested[String] = ???
  }

  val example1 = simpleVersion.apply(simplify)
  println(s"Example 1: ${example1.apply(interpret)} = ${example1.apply(interpretAsPrettyPrint)}")

  val example2 = new ScalaToLanguageBridge[Int] {
    override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = {
      // ((0 + 1) + [0 + 1 + 1]) + 1
      L.increment(L.add(L.increment(L.number(0)), L.increment(L.increment(L.number(0)))))
    }
  }
  println(s"Example 2: ${example2.apply(interpret)} = ${example2.apply(interpretAsPrettyPrint)}")
  println(s"Example 2: ${example2.apply(simplify).apply(interpret)} = ${example2.apply(simplify).apply(interpretAsPrettyPrint)}")
}
