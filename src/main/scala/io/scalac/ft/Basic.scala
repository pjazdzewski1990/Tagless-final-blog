package io.scalac.ft


object Basic extends App {
  trait Language[Wrapper[_]] {
    def number(v: Int): Wrapper[Int]
    def increment(a: Wrapper[Int]): Wrapper[Int]
    def add(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]

    def text(v: String): Wrapper[String]
    def toUpper(a: Wrapper[String]): Wrapper[String]
    def concat(a: Wrapper[String], b: Wrapper[String]): Wrapper[String]

    def toString(v: Wrapper[Int]): Wrapper[String]
  }

  trait ScalaToLanguageBridge[ScalaValue] {
    def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[ScalaValue]
  }

  def buildNumber(number: Int) = new ScalaToLanguageBridge[Int] {
    override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = L.number(number)
  }
  def buildIncrementNumber(number: Int) = new ScalaToLanguageBridge[Int] {
    override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = L.increment(L.number(number))
  }
  def buildIncrementExpression(expression: ScalaToLanguageBridge[Int]) = new ScalaToLanguageBridge[Int] {
    override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Int] = L.increment(expression.apply)
  }
  val basicExpression = buildIncrementExpression(buildIncrementNumber(0))

  // builds an expression like: println(s"$text ${a + (b + 1)}")
  def buildComplexExpression(text: String, a: Int, b: Int) = new ScalaToLanguageBridge[String] {
    override def apply[Wrapper[_]](implicit F: Language[Wrapper]): Wrapper[String] = {
      val addition = F.add(F.number(a), F.increment(F.number(b)))
      F.concat(F.text(text), F.toString(addition))
    }
  }
  val fullExpression = buildComplexExpression("Result is ", 10, 1)


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

  println(s"interpreted basic: ${basicExpression.apply(interpret)}")
  println(s"interpreted full: ${fullExpression.apply(interpret)}")

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

  println(s"interpreted basic (as pretty print): ${basicExpression.apply(interpretAsPrettyPrint)}")
  println(s"interpreted full (as pretty print): ${fullExpression.apply(interpretAsPrettyPrint)}")

  import scala.concurrent.{Await, Future}
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val interpretAsAsync = new Language[Future] {
    override def number(v: Int): Future[Int] = Future.successful(v)
    override def increment(a: Future[Int]): Future[Int] =
      for {
        av <- a
      } yield av+1
    override def add(a: Future[Int], b: Future[Int]): Future[Int] =
      for {
        av <- a
        bv <- b
      } yield av+bv

    override def text(v: String): Future[String] = Future.successful(v)
    override def toUpper(a: Future[String]): Future[String] =
      for {
        av <- a
      } yield av.toUpperCase

    override def concat(a: Future[String], b: Future[String]): Future[String] =
      for {
        av <- a
        bv <- b
      } yield av+bv

    override def toString(v: Future[Int]): Future[String] = v.map(_.toString)
  }

  println(s"interpreted basic (as Future): ${Await.result(basicExpression.apply(interpretAsAsync), 1.second)}")
  println(s"interpreted full (as Future): ${Await.result(fullExpression.apply(interpretAsAsync), 1.second)}")
}
