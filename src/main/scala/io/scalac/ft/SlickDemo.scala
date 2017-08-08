package io.scalac.ft

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object SlickDemo extends App{

  case class Person(id: Int, name: String, yob: Int)

  trait Language[Wrapper[_]] {
    type QueryObj
    case class Raw(q: QueryObj)
    case class WithFilter(q: QueryObj)
    case class WithPagination(q: QueryObj)

    def people(): Wrapper[Raw]
    // we could change plain Ints into our custom class, but will skip it for brevity
    def filterByIds(query: Wrapper[Raw], ids: Seq[Int]): Wrapper[WithFilter]
    def paginate(query: Wrapper[WithFilter], skip: Int, limit: Int): Wrapper[WithPagination]

    def run(query: Wrapper[WithPagination]): Wrapper[Seq[Person]]
  }

  trait ScalaToLanguageBridge[ScalaValue] {
    def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[ScalaValue]
  }

  val slickInterpreter = new Language[Future] {
    import slick.jdbc.H2Profile.api._

    class SlickPersonTable(tag: Tag) extends Table[Person](tag, "people") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def yob = column[Int]("yob")
      def * = (id, name, yob) <> (Person.tupled, Person.unapply)
    }
    private val slickPersonQuery = TableQuery[SlickPersonTable]
    private val db = Database.forURL(
      url = "jdbc:h2:mem:test1",
      driver = "org.h2.Driver",
      keepAliveConnection = true
    )
    val setup = DBIO.seq(
      slickPersonQuery.schema.create,
      slickPersonQuery ++= Seq(
        Person(1, "person 1", 1980),
        Person(2, "person 2", 1990),
        Person(3, "person 3", 2000)
      )
    )
    val created = Await.result(db.run(setup), 10.seconds)
    println(s"Creation result $created ")

    override type QueryObj = Query[SlickPersonTable, Person, Seq]

    override def people(): Future[Raw] = {
      Future.successful(Raw(slickPersonQuery))
    }
    override def filterByIds(query: Future[Raw], ids: Seq[Int]): Future[WithFilter] = {
      query.map(_.q.filter(_.id inSet ids)).map(WithFilter)
    }
    override def paginate(query: Future[WithFilter], skip: Int, limit: Int): Future[WithPagination] = {
      query.map(_.q.drop(skip).take(limit)).map(WithPagination)
    }
    override def run(query: Future[WithPagination]): Future[Seq[Person]] = {
      query.flatMap { case finalQuery =>
        db.run(finalQuery.q.result)
      }
    }
  }

  val findMiddleUser = new ScalaToLanguageBridge[Seq[Person]] {
    override def apply[Wrapper[_]](implicit L: Language[Wrapper]): Wrapper[Seq[Person]] = {
      val base = L.people()
      val full = L.paginate(L.filterByIds(base, Seq(1,2,3)), skip = 1, limit = 1)
      L.run(full)
    }
  }
  val result = Await.result(findMiddleUser.apply(slickInterpreter), 10.seconds)
  println(s"Query result is $result")
}
