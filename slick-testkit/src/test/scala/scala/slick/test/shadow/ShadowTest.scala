package scala.slick.test.shadow

import scala.language.implicitConversions
import org.junit.Test
import org.junit.Assert._
import scala.slick.shadow._
import scala.slick.driver.H2Driver
import scala.slick.jdbc.JdbcBackend

//@Entity("COFFEE") case class CoffeeNotNested(@Entity("ID") idNumber: Int, @Entity("NAME") coffeeName: String)
//object NestingObject {
//  @Entity("COFFEE") case class CoffeeNested1(@Entity("ID") idNumber: Int, @Entity("NAME") coffeeName: String)
//  object Level2 {
//    @Entity("COFFEE") case class CoffeeNested2(@Entity("ID") idNumber: Int, @Entity("NAME") coffeeName: String)
//  }
//}

class ShadowTest {
  import scala.slick.shadow.test.YYDefinitions._

  @Test def simpleTest() {
    import Shallow._
    import Shallow.TestH2._
    val y = 5.3
    val r1 = stage {
      val q = Query(y)
      q
    }.list()
    assertEquals("Query of int", y, r1.head, 0.1)
    val r2 = stage {
      Query(y).map(x => x)
    }.list()
    assertEquals("Query identity map", y, r2.head, 0.1)
    val r3 = stage {
      Query(y).map(x => false)
    }.list()
    assertEquals("Query dummy map", false, r3.head)
    val r4 = stage {
      Query(y).filter(x => x < 1)
    }.list()
    assertEquals("Query filter", 0, r4.length)

    val z = 3
    val r5 = stage {
      Query(z).filter(x => x > 2.5)
    }.list()
    assertEquals("Query filter + captured var", z, r5.head)
    val a = 1
    val r6 = stage {
      val b = 1
      Query(a).filter(x => x == b)
    }.list()
    assertEquals("Query filter + Column ==", a, r6.head)
    val r7 = stage {
      val b = 1
      Query(2 > b).filter(x => x == true)
    }.list()
    assertEquals("Query filter + Column == (2)", true, r7.head)
  }
  @Test
  def tuple2Test() {
    import Shallow._
    import Shallow.TestH2._
    val r1 = stage {
      val x = (1, 2)
      val q = Query(x)
      //      q.first
      q
    }.first
    assertEquals("Query of tuple2", (1, 2), r1)
    val r2 = stage {
      val x = (1, 2.5)
      //      Query(x).map(x => x._2).first
      Query(x).map(x => x._2)
    }.first
    assertEquals("Query map _2 of tuple", 2.5, r2, 0.1)
    val r3 = stage {
      val x = (1, 2)
      val q = Query(x).map(x => x._2)
      val q2 = q.filter(x => x == 1)
      q2
    }.list()
    assertEquals("Query filter of tuple2 + Column ==", 0, r3.length)
    val r4 = stage {
      val x = (1, 2)
      Query(x).map(x => x._2).filter(x => x == 2)
    }.list()
    assertEquals("Query filter of tuple2 + Column == (2)", 1, r4.length)
    val r5 = stage {
      val x = (1, 2)
      Query(x).map(x => x._2).filter(x => x > 1)
    }.list()
    assertEquals("Query filter of tuple2 + Column >", 1, r5.length)
    val r6 = stage {
      Query((1, 2)).map(x => (x._2, if (x._2 == 2) false else true))
    }.list()
    assertEquals("Query map of tuple 2 + Column > + if true", (2, false), r6.head)
    val r7 = stage {
      Query((1, 2)).map(x => (x._2, if (x._2 == 1) false else true))
    }.list()
    assertEquals("Query map of tuple 2 + Column > + if false", (2, true), r7.head)
  }

  @Test
  def tuple22Test() {
    import Shallow._
    import Shallow.TestH2._
    val r1 = stage {
      val x = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
      val q = Query(x)
      //      q.first
      q
    }.first
    assertEquals("Query of tuple22", (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22), r1)
  }

  @Test
  def virtualizationProTest {
    initCoffeeTable()
    import Shallow._
    import Shallow.TestH2._

    val r1 = stage {
      val tbl = Table.getTable[Coffee1]
      val q = Query.ofTable(tbl)
      val q1 = q map (x => x.id)
      q1
    }.list()
    assertEquals("Query map _1 of Virtualized++ Table", 4, r1.length)
    val r2 = stage {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name))
      q1
    }.list()
    assertEquals("Query map (_1, _2) of Virtualized++ Table", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r2.toList)
    val r3 = stage {
      val q1 = Queryable[Coffee1] map (x => (x.id, if (x.id < 3) "Low" else x.name))
      q1
    }.list()
    assertEquals("Query map (_1, _2) of Virtualized++ Table + if", List((1, "Low"), (2, "Low"), (3, "three"), (10, "ten")), r3.toList)
    val r4 = stage {
      val q1 = Queryable[Coff] map (x => (x.idNumber, x.name))
      q1
    }.list()
    assertEquals("Query map (_1, _2) of Virtualized++ Table + Annotation", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r4.toList)
    val r5 = stage {
      val q1 = Queryable[Coff] map (x => x.idNumber) filter (x => x < 3)
      q1
    }.list()
    assertEquals("Query map _1 filter of Virtualized++ Table + Annotation", List(1, 2), r5.toList)
    val r6 = stage {
      val q1 = Queryable[Coff] map (x => (x.idNumber, x.name)) filter (x => x._1 < 3)
      q1
    }.list()
    assertEquals("Query map (_1, _2) filter of Virtualized++ Table + Annotation", List((1, "one"), (2, "two")), r6.toList)

    val r7 = stage {
      val q1 = Queryable[Coffn] filter (x => x.idNumber == 3) map (x => (x.idNumber, x._2))
      q1
    }.list()
    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", List((3, "three")), r7.toList)
    DatabaseHandler.closeSession
  }

  //  @Test
  //  def virtualizationOutsideTest {
  //    initCoffeeTable()
  //    import Shallow._
  //    import Shallow.TestH2._
  //    val r1 = stage {
  //      val q1 = Queryable[CoffeeNotNested] filter (x => x.idNumber == 3)
  //      q1.first
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNotNested(3, "three"), r1)
  //    val r2 = stage {
  //      val q1 = Queryable[CoffeeNotNested] filter (x => x.idNumber == 3)
  //      q1.toSeq
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNotNested(3, "three"), r2.head)
  //
  //    import NestingObject.CoffeeNested1
  //
  //    val r3 = stage {
  //      val q1 = Queryable[CoffeeNested1] filter (x => x.idNumber == 3)
  //      q1.first
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNested1(3, "three"), r3)
  //    val r4 = stage {
  //      val q1 = Queryable[CoffeeNested1] filter (x => x.idNumber == 3)
  //      q1.toSeq
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNested1(3, "three"), r4.head)
  //
  //    import NestingObject.Level2.CoffeeNested2
  //
  //    val r5 = stage {
  //      val q1 = Queryable[CoffeeNested2] filter (x => x.idNumber == 3)
  //      q1.first
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNested2(3, "three"), r5)
  //    val r6 = stage {
  //      val q1 = Queryable[CoffeeNested2] filter (x => x.idNumber == 3)
  //      q1.toSeq
  //    }
  //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNested2(3, "three"), r6.head)
  //
  //    //    @Entity("COFFEE") case class CoffeeNested3(@Entity("ID") idNumber: Int, @Entity("NAME") coffeeName: String)
  //    //    val r7 = stage {
  //    //      val q1 = Queryable[CoffeeNested3] filter (x => x.idNumber == 3)
  //    //      q1.first
  //    //    }
  //    //    val r8 = stage {
  //    //      val q1 = Queryable[CoffeeNested3] filter (x => x.idNumber == 3)
  //    //      q1.toSeq
  //    //    }
  //    //    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", CoffeeNested3(3, "three"), r8.head)
  //
  //    DatabaseHandler.closeSession
  //  }

  @Test
  def sortTest {
    initSortTable()
    import Shallow._
    import Shallow.TestH2._
    val r1 = stage {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name)) sortBy (x => x._2)
      q1
    }.list()
    assertEquals("Query sort by name of Table", List((2, "one"), (1, "one"), (10, "ten"), (3, "three")), r1.toList)
    val r2 = stage {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name)) sortBy (x => x._1)
      q1
    }.list()
    assertEquals("Query sort by id of Table", List((1, "one"), (2, "one"), (3, "three"), (10, "ten")), r2.toList)
    val r3 = stage {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name)) sortBy (x => (x._2, x._1))
      q1
    }.list()
    assertEquals("Query sort by (name, id) of Table", List((1, "one"), (2, "one"), (10, "ten"), (3, "three")), r3.toList)
    val r4 = stage {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name)) sortBy (x => (x._2, x._1)) take 2
      q1
    }.list()
    assertEquals("Query sort by (name, id) + take of Table", List((1, "one"), (2, "one")), r4.toList)
    val r5 = stage {
      val q1 = Queryable[Coffee1] map (x => (x.id, x.name)) sortBy (x => (x._1, x._2)) drop 1
      q1
    }.list()
    assertEquals("Query sort by (id, name) + drop of Table", List((2, "one"), (3, "three"), (10, "ten")), r5.toList)

    val r6 = stage {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => x._1)(Ordering[Int])
      q1
    }.list()
    assertEquals("Query sort by id of Table + Ordering", List((1, "one"), (2, "one"), (3, "three"), (10, "ten")), r6.toList)
    val r7 = stage {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => x._1)(Ordering[Int].reverse)
      q1
    }.list()
    assertEquals("Query sort by reverse of id of Table + Ordering", List((10, "ten"), (3, "three"), (2, "one"), (1, "one")), r7.toList)
    val r8 = stage {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => x._2)(Ordering[String].reverse)
      q1
    }.list()
    assertEquals("Query sort by reverse of name of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r8.toList)
    val r9 = stage {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => (x._2, x._1))(Ordering[(String, Int)])
      q1
    }.list()
    assertEquals("Query sort by (name, id) of Table + Ordering", List((1, "one"), (2, "one"), (10, "ten"), (3, "three")), r9.toList)
    val r10 = stage {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => (x._2, x._1))(Ordering[(String, Int)].reverse)
      q1
    }.list()
    assertEquals("Query sort by reverse of (name, id) of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r10.toList)
    val r11 = stage {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sortBy(x => (x._2, x._1))(Ordering.by[(String, Int), String](_._1).reverse)
      q1
    }.list()
    assertEquals("Query sort by reverse of name of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r11.toList)
    val r12 = stage {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sorted(Ordering.by[(Int, String), String](_._2).reverse)
      q1
    }.list()
    assertEquals("Query sorted reverse of name of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r12.toList)
    val r13 = stage {
      val q1 = Queryable[Coffee1].map(x => (x.id, x.name)).sorted(Ordering.by[(Int, String), (String, Int)](x => (x._2, x._1)).reverse)
      q1
    }.list()
    assertEquals("Query sorted by reverse of (name, id) of Table + Ordering", List((3, "three"), (10, "ten"), (2, "one"), (1, "one")), r13.toList)

    DatabaseHandler.closeSession
  }

  @Test
  def forComprehensionTest() {
    initCoffeeTable()
    import Shallow._
    import Shallow.TestH2._
    val r1 = stage {
      val tbl = Table.getTable[Coffee1]
      val q = Query.ofTable(tbl)
      val q1 = for (x <- q) yield x.id
      q1
    }.list()
    assertEquals("Query forComprehension map _1 of Virtualized Table", 4, r1.length)
    val r2 = stage {
      val q1 = for (x <- Queryable[Coffee1]) yield (x.id, x.name)
      q1
    }.list()
    assertEquals("Query forComprehension map (_1, _2) of Table", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r2.toList)
    val r3 = stage {
      val q1 = for (x <- Queryable[Coffee1]) yield (x.id, if (x.id < 3) "Low" else x.name)
      q1
    }.list()
    assertEquals("Query forComprehension map (_1, _2) of Table + if", List((1, "Low"), (2, "Low"), (3, "three"), (10, "ten")), r3.toList)
    val r4 = stage {
      val q1 = for (x <- Queryable[Coff]) yield (x.idNumber, x.name)
      q1
    }.list()
    assertEquals("Query forComprehension map (_1, _2) of Table + Annotation", List((1, "one"), (2, "two"), (3, "three"), (10, "ten")), r4.toList)
    val r5 = stage {
      val q1 = for (x <- Queryable[Coff] if x.idNumber < 3) yield x.idNumber
      q1
    }.list()
    assertEquals("Query forComprehension map _1 filter of Table + Annotation", List(1, 2), r5.toList)
    val r6 = stage {
      val q1 = for (x <- Queryable[Coff] if x.idNumber < 3) yield (x.idNumber, x.name)
      q1
    }.list()
    assertEquals("Query forComprehension map (_1, _2) filter of Table + Annotation", List((1, "one"), (2, "two")), r6.toList)
    val r7 = stage {
      val q1 = for (x <- Queryable[Coffn] if x.idNumber == 3) yield (x.idNumber, x._2)
      q1
    }.list()
    assertEquals("Query forComprehension filter == map (_1, _2) of Table + Annotation", List((3, "three")), r7.toList)
    val r8 = stage {
      val q1 = for ((x, y) <- Queryable[Coffn].map(x => (x.idNumber, x._2)) if x == 3) yield (x, y)
      q1
    }.list()
    assertEquals("Query forComprehension filter == map (_1, _2) of Table + Annotation + (_1, _2) <-", List((3, "three")), r8.toList)
    val r9 = stage {
      val q1 = for (x <- Queryable[Coff]) yield (x.idNumber, (x.idNumber, x.name))
      q1
    }.list()
    assertEquals("Query forComprehension map NestedTuples (_1, (_1, _2)) filter of Table + Annotation", List((1, (1, "one")), (2, (2, "two")), (3, (3, "three")), (10, (10, "ten"))), r9.toList)
    DatabaseHandler.closeSession
  }
  //  @Test
  //  def virtualizationProInvokerTest {
  //    initCoffeeTable()
  //    import Shallow._
  //    def driver = H2Driver
  //    implicit val session = DatabaseHandler.provideSession
  //    val r1 = stage {
  //      val q1 = Queryable[Coffee1] map (x => x.id)
  //      q1.getInvoker
  //    }(driver)
  //    assertEquals("Query map _1 of Virtualized++ Table invoker", 4, r1.list.length)
  //    val r2 = stage {
  //      val q1 = Queryable[Coffee1] map (x => x.id)
  //      q1.toSeqImplicit
  //    }(driver)(session)
  //    assertEquals("Query map _1 of Virtualized++ Table toSeqImplicit", 4, r2.length)
  //    DatabaseHandler.closeSession
  //  }

  @Test
  def columnOpsTest {
    initCoffeeTable()
    import Shallow._
    import Shallow.TestH2._
    val r1 = stage {
      val q1 = Queryable[Coffee1] map (x => x.id + 2)
      q1
    }.list()
    assertEquals("numericOps +", List(3, 4, 5, 12), r1.toList)
    val r2 = stage {
      val q1 = Queryable[Coffee1] map (x => x.id * 2 % 3)
      q1
    }.list()
    assertEquals("numericOps * %", List(2, 1, 0, 2), r2.toList)
    val r3 = stage {
      val q1 = Queryable[Coffee1] map (x => ((x.id - 5).abs, (x.id).toDegrees))
      q1
    }.list()
    assertEquals("numericOps (x - 5).abs, toDegrees", List((4, 57), (3, 115), (2, 172), (5, 573)), r3.toList)
    val r4 = stage {
      val q1 = Queryable[Coffee1] map (x => (x.name + "!"))
      q1
    }.list()
    assertEquals("stringOps +", List("one!", "two!", "three!", "ten!"), r4.toList)
    val r5 = stage {
      val q1 = Queryable[Coffee1] map (x => (x.name ++ "!").toUpperCase)
      q1
    }.list()
    assertEquals("stringOps ++ toUpperCase", List("ONE!", "TWO!", "THREE!", "TEN!"), r5.toList)
    val r6 = stage {
      val q1 = Queryable[Coffee1] map (x => if (x.name like "%e") x.name.toUpperCase else ("  " + x.name + "! ").trim)
      q1
    }.list()
    assertEquals("stringOps if (like %%e) toUpperCase else ( + + ).trim", List("ONE", "two!", "THREE", "ten!"), r6.toList)
    val r7 = stage {
      val q1 = Queryable[Coffee1] map (x => if (x.name like "%e") ("  " + x.name + "!  ").ltrim else ("  " + x.name + "!  ").rtrim)
      q1
    }.list()
    assertEquals("stringOps if (like %%e) ( + + ).ltrim else ( + + ).rtrim", List("one!  ", "  two!", "three!  ", "  ten!"), r7.toList)
    val r8 = stage {
      val q1 = Queryable[Coffee1] map (x => if (x.name endsWith "e") x.name.toUpperCase else ("  " + x.name + "! ").trim)
      q1
    }.list()
    assertEquals("stringOps if (endsWith 'e') toUpperCase else ( + + ).trim", List("ONE", "two!", "THREE", "ten!"), r8.toList)
    DatabaseHandler.closeSession
  }
  @Test
  def testJoin {
    import Shallow.TestH2._
    {
      import scala.slick.driver.H2Driver.simple._
      object Categories extends Table[(Int, String)]("cat_j") {
        def id = column[Int]("id")
        def name = column[String]("name")
        def * = id ~ name
      }

      object Posts extends Table[(Int, String, Int)]("posts_j") {
        def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def title = column[String]("title")
        def category = column[Int]("category")
        def * = id ~ title ~ category
      }

      (Categories.ddl ++ Posts.ddl).create

      Categories insertAll (
        (1, "Scala"),
        (2, "ScalaQuery"),
        (3, "Windows"),
        (4, "Software"))
      Posts.title ~ Posts.category insertAll (
        ("Test Post", -1),
        ("Formal Language Processing in Scala, Part 5", 1),
        ("Efficient Parameterized Queries in ScalaQuery", 2),
        ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
        ("A ScalaQuery Update", 2))
    }

    import Shallow._

    val q1 = stage {
      (for {
        c <- Queryable[Categories]
        p <- Queryable[Posts] if c.id == p.category
      } yield (p.id, c.id, c.name, p.title)).sortBy(_._1).map(x => (x._1, x._2))
    }.list()
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2)), q1.toList)

    val q2 = stage {
      val q = Queryable[Categories] innerJoin Queryable[Posts] on (_.id == _.category)
      val q1 = q.sortBy(_._2.id)
      val q2 = q1.map(x => (x._2.id, x._1.id))
      q2
    }.list()
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2)), q2.toList)

    val q3 = stage {
      (for {
        (c, p) <- Queryable[Categories] leftJoin Queryable[Posts] on (_.id == _.category)
      } yield (p.id, (p.id.?.getOrElse(0), c.id, c.name, p.title.?.getOrElse("")))).sortBy(x => x._1)(nullsFirst).map(_._2).map(x => (x._1, x._2))
    }.list()
    assertEquals(List((0, 4), (2, 1), (3, 2), (4, 3), (5, 2)), q3.toList)

    assertFail {
      stage {
        (for {
          (c, p) <- Queryable[Categories] leftJoin Queryable[Posts] on (_.id == _.category)
        } yield (p.id, c.id, c.name, p.title)).sortBy(_._1)(nullsFirst)
      }.list()
    }

    val q3b = stage {
      (for {
        (c, p) <- Queryable[Categories] leftJoin Queryable[Posts] on (_.id == _.category)
      } yield (p.id, (p.id.?.getOrElse(0), c.id, c.name, p.title.?.getOrElse("")))).sortBy(_._1)(nullsLast).map(_._2).map(x => (x._1, x._2))
    }.list()
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2), (0, 4)), q3b.toList)

    val q4 = stage {
      (for {
        (c, p) <- Queryable[Categories] rightJoin Queryable[Posts] on (_.id == _.category)
      } yield (p.id, c.id.?.getOrElse(0), c.name.?.getOrElse(""), p.title)).sortBy(_._1).map(x => (x._1, x._2))
    }.list()
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3), (5, 2)), q4.toList)
    DatabaseHandler.closeSession
  }

  @Test
  def testZip = {
    import Shallow.TestH2._
    {
      import scala.slick.driver.H2Driver.simple._
      object Categories extends Table[(Int, String)]("cat_j") {
        def id = column[Int]("id")
        def name = column[String]("name")
        def * = id ~ name
      }

      object Posts extends Table[(Int, String, Int)]("posts_j") {
        def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def title = column[String]("title")
        def category = column[Int]("category")
        def * = id ~ title ~ category
      }

      (Categories.ddl ++ Posts.ddl).create

      Categories insertAll (
        (1, "Scala"),
        (3, "Windows"),
        (2, "ScalaQuery"),
        (4, "Software"))
      Posts.title ~ Posts.category insertAll (
        ("Test Post", -1),
        ("Formal Language Processing in Scala, Part 5", 1),
        ("Efficient Parameterized Queries in ScalaQuery", 2),
        ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
        ("A ScalaQuery Update", 2))
    }

    import Shallow._
    val q1 = stage {
      val q = Queryable[Categories].sortBy(_.id).zipWithIndex
      val q1 = q.map(x => (x._1.id, x._2))
      q1
    }.list()
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), q1.toList)
    val q2 = stage {
      val q =
        Queryable[Categories].sortBy(_.id) zip Queryable[Posts].sortBy(_.category)
      val q1 = q.map(x => (x._1.id, x._2.category))
      q1
    }.list()
    assertEquals(List((1, -1), (2, 1), (3, 2), (4, 2)), q2.toList)
    val q3 = stage {
      val q = for {
        (c, p) <- Queryable[Categories].sortBy(_.id) zip Queryable[Posts].sortBy(_.category)
      } yield (c.id, p.category)
      q
    }.list()
    assertEquals(List((1, -1), (2, 1), (3, 2), (4, 2)), q3.toList)
    DatabaseHandler.closeSession
  }

  @Test
  def testGroupBy = {
    import Shallow.TestH2._
    {
      import scala.slick.driver.H2Driver.simple._
      object T extends Table[(Int, Int)]("t3") {
        def a = column[Int]("a")
        def b = column[Int]("b")
        def * = a ~ b
      }
      T.ddl.create
      T.insertAll((1, 1), (1, 2), (1, 3))
      T.insertAll((2, 1), (2, 2), (2, 5))
      T.insertAll((3, 1), (3, 9))
    }
    import Shallow._
    val r0 = stage {
      val q0 = Queryable[T3].groupBy(_.a)
      val q1 = q0.map(_._2.length).sorted
      q1
    }.list()
    val r0t: List[Int] = r0.toList
    assertEquals(List(2, 3, 3), r0t)
    val r = stage {
      (for {
        (k, v) <- Queryable[T3].groupBy(t => t.a)
      } yield (k, v.length)).sortBy(_._1)
    }.list()
    val rt: List[(Int, Int)] = r.toList
    assertEquals(List((1, 3), (2, 3), (3, 2)), rt)

    val r2 = stage {
      (
        (for {
          (k, v) <- Queryable[T3].groupBy(t => t.a)
        } yield (k, v.length, v.map(_.a).sum, v.map(_.b).sum)).sortBy(_._1)
      )
    }.list()
    val r2t: List[(Int, Int, Option[Int], Option[Int])] = r2.toList
    assertEquals(List((1, 3, Some(3), Some(6)), (2, 3, Some(6), Some(8)), (3, 2, Some(6), Some(10))), r2t)
    DatabaseHandler.closeSession
  }

  @Test
  def testOption = {
    import Shallow.TestH2._
    val inMemT3 = List((1, Some(1)), (-1, Some(2)), (1, Some(3)),
      (2, Some(1)), (2, Some(2)), (-2, Some(5)),
      (-3, Some(1)), (3, None)
    )

    {
      import scala.slick.driver.H2Driver.simple._
      object T extends Table[(Int, Option[Int])]("t3o") {
        def a = column[Int]("a")
        def b = column[Option[Int]]("b")
        def * = a ~ b
      }
      T.ddl.create
      T.insertAll(inMemT3: _*)
    }
    import Shallow._
    val r0 = stage {
      Queryable[T3O].map(x => (x.a, x.b))
    }.list()
    assertEquals(inMemT3, r0.toList)
    val r1 = stage {
      Queryable[T3O].map(x => (x.a, x.b.getOrElse(0)))
    }.list()
    assertEquals(inMemT3 map {
      case (_1, _2) => (_1, _2.getOrElse(0))
    }, r1.toList)
    assertEquals(inMemT3, r0.toList)
    val r2 = stage {
      Queryable[T3O].map(x => (x.a.?, x.b.getOrElse(0)))
    }.list()
    assertEquals(inMemT3 map {
      case (_1, _2) => (Some(_1), _2.getOrElse(0))
    }, r2.toList)
    val r3 = stage {
      Queryable[T3O].map(x => (x.a.?.getOrElse(3), x.b.getOrElse(0)))
    }.list()
    assertEquals(inMemT3 map {
      case (_1, _2) => (_1, _2.getOrElse(0))
    }, r3.toList)
    val r4 = stage {
      Queryable[T3O].map(x => (x.a.?.toRadians, x.b.getOrElse(0)))
    }.list()
    assertEquals(inMemT3 map {
      case (_1, _2) => (Some(0), _2.getOrElse(0))
    }, r4.toList)
    DatabaseHandler.closeSession
  }

  @Test
  def composabilityTest {
    initCoffeeTable()
    import Shallow._
    import Shallow.TestH2._

    {
      import scala.slick.driver.H2Driver.simple._
      object Categories extends Table[(Int, String)]("cat_j") {
        def id = column[Int]("id")
        def name = column[String]("name")
        def * = id ~ name
      }

      object Posts extends Table[(Int, String, Int)]("posts_j") {
        def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def title = column[String]("title")
        def category = column[Int]("category")
        def * = id ~ title ~ category
      }

      (Categories.ddl ++ Posts.ddl).create

      Categories insertAll (
        (1, "Scala"),
        (2, "ScalaQuery"),
        (3, "Windows"),
        (4, "Software"))
      Posts.title ~ Posts.category insertAll (
        ("Test Post", -1),
        ("Formal Language Processing in Scala, Part 5", 1),
        ("Efficient Parameterized Queries in ScalaQuery", 2),
        ("Removing Libraries and HomeGroup icons from the Windows 7 desktop", 3),
        ("A ScalaQuery Update", 2))

      object T extends Table[(Int, Int)]("t3") {
        def a = column[Int]("a")
        def b = column[Int]("b")
        def * = a ~ b
      }
      T.ddl.create
      T.insertAll((1, 1), (1, 2), (1, 3))
      T.insertAll((2, 1), (2, 2), (2, 5))
      T.insertAll((3, 1), (3, 9))
    }

    val q: Query[Coffee1] = stage {
      Queryable[Coffee1] filter (x => x.id == 3)
    }
    val r7 = stage {
      q
    }.list()
    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", Coffee1(3, "three"), r7.head)

    val qq = stage {
      Queryable[Coffee1] map (x => x.id)
    }
    val r8 = stage {
      (qq filter (x => x < 3))
    }.list()
    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", List(1, 2), r8.toList)

    val q0 = stage {
      Queryable[Coffee1]
    }
    val q1 = stage {
      q0 filter (x => x.id == 3)
    }
    val r9 = stage {
      q1
    }.list()
    assertEquals("Query filter == map (_1, _2) of Virtualized++ Table + Annotation", Coffee1(3, "three"), r9.head)

    val categories = stage {
      Queryable[Categories]
    }
    val posts = stage {
      Queryable[Posts]
    }

    val cpImplicitJoin = stage {
      for {
        c <- Queryable[Categories]
        p <- Queryable[Posts] if c.id == p.category
      } yield (p.id, c.id, c.name, p.title)
    }

    val cp1 = stage {
      (cpImplicitJoin).sortBy(_._1).map(x => (x._1, x._2))
    }.list()
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2)), cp1.toList)

    val cpInnerJoin = stage {
      categories innerJoin posts on (_.id == _.category)
    }
    val cpInnerJoinSorted = stage {
      cpInnerJoin.sortBy(_._2.id)
    }
    val cpInnerJoinMapped = stage {
      cpInnerJoinSorted.map(x => (x._2.id, x._1.id))
    }
    val cp2 = stage {
      cpInnerJoinMapped
    }.list()
    assertEquals(List((2, 1), (3, 2), (4, 3), (5, 2)), cp2.toList)

    val cpRightJoin = stage {
      categories rightJoin posts on (_.id == _.category)
    }
    val cpRightJoinMapped = stage {
      for {
        (c, p) <- cpRightJoin
      } yield (p.id, c.id.?.getOrElse(0), c.name.?.getOrElse(""), p.title)
    }
    val cpRightJoinSorted = stage {
      cpRightJoinMapped.sortBy(_._1)
    }
    val cpRightJoinSortedMapped = stage {
      cpRightJoinSorted.map(x => (x._1, x._2))
    }
    val cp3 = stage {
      cpRightJoinSortedMapped
    }.list()
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3), (5, 2)), cp3.toList)

    val t3Query = stage {
      Queryable[T3]
    }
    val t3GroupBy = stage {
      t3Query.groupBy(t => t.a)
    }
    val t3GroupByMapped = stage {
      for {
        (k, v) <- t3GroupBy
      } yield (k, v.length, v.map(_.a).sum, v.map(_.b).sum)
    }
    val t3GroupBySorted = stage {
      t3GroupByMapped.sortBy(_._1)
    }
    val t3r = stage {
      t3GroupBySorted
    }.list()
    val t3result: List[(Int, Int, Option[Int], Option[Int])] = t3r.toList
    assertEquals(List((1, 3, Some(3), Some(6)), (2, 3, Some(6), Some(8)), (3, 2, Some(6), Some(10))), t3result)
    DatabaseHandler.closeSession
  }

  @Test
  def insersionTest {
    initCoffeeTable()
    import Shallow._
    import Shallow.TestH2._

    val cId = 1989
    val cName = "Amir"
    val coffee = Coffee1(cId, cName)

    val r = stage {
      val c = Coffee1(1989, "Amir")
      c
    }
    assertEquals("Creating case class object inside block", coffee, r)

    val r1Before = stage {
      Queryable[Coffee1]
    }.list()

    //    stage {
    //      val c = Coffee1(1989, "Amir")
    //      Queryable[Coffee1].insert(c)
    //    }
    stage {
      Queryable[Coffee1]
    }.insert(coffee)

    //    stage {
    //      val c = Coffee1(1989, "Amir")
    //      Queryable[Coffee1].insert(c)
    //    }
    stage {
      Queryable[Coffee1]
    }.insert(coffee)

    val r1After = stage {
      Queryable[Coffee1]
    }.list()

    assertEquals("Inserting a constant element", r1Before ++ List(coffee, coffee), r1After)

    val c1 = Coffee1(cId, cName)
    stage {
      //      val c = Coffee1(cId, cName)
      //      Queryable[Coffee1].insert(c)
      Queryable[Coffee1]
    }.insert(c1)

    val r2After = stage {
      Queryable[Coffee1]
    }.list()

    assertEquals("Inserting an element using captured variables", r1After ++ List(coffee), r2After)

    DatabaseHandler.closeSession
  }

  @Test
  def updateTest {
    initCoffeeTable()
    import Shallow._
    import Shallow.TestH2._

    def getAll() = stage {
      Queryable[Coffee1]
    }.list()

    val cId = 1989
    val cName = "Amir"
    val coffee = Coffee1(cId, cName)

    val r1Before = getAll()

    //    stage {
    //      val c = Coffee1(1989, "Amir")
    //      Queryable[Coffee1].insert(c)
    //    }
    stage {
      Queryable[Coffee1]
    }.insert(coffee)

    //    stage {
    //      val c = Coffee1(1368, "Amir")
    //      Queryable[Coffee1].filter(_.id == 1989).update(c)
    //    }
    stage {
      Queryable[Coffee1].filter(_.id == 1989)
    }.update(Coffee1(1368, "Amir"))

    val r1After = getAll()

    assertEquals("Updating a constant element with constant value", r1Before ++ List(Coffee1(1368, cName)), r1After)

    //    def updateToGivenId(toId: Int) = stage {
    //      val c = Coffee1(toId, "Amir")
    //      Queryable[Coffee1].filter(_.id == 1368).update(c)
    //    }
    def updateToGivenId(toId: Int) = stage {
      Queryable[Coffee1].filter(_.id == 1368)
    }.update(Coffee1(toId, "Amir"))

    updateToGivenId(cId)

    val r2After = getAll()

    assertEquals("Updating a constant element with captured value", r1Before ++ List(Coffee1(cId, cName)), r2After)

    //    def updateById(id: Int) = stage {
    //      val c = Coffee1(1368, "Amir")
    //      Queryable[Coffee1].filter(_.id == id).update(c)
    //    }
    def updateById(id: Int) = stage {
      Queryable[Coffee1].filter(_.id == id)
    }.update(Coffee1(1368, "Amir"))

    updateById(cId)

    val r3After = getAll()

    assertEquals("Updating a captured element with constant value", r1Before ++ List(Coffee1(1368, cName)), r3After)
    //    def updateByIdToId(id: Int, toId: Int, toName: String) = stage {
    //      val i = id // inorder to change the order of holes
    //      val c = Coffee1(toId, toName)
    //      Queryable[Coffee1].filter(_.id == i).update(c)
    //    }
    def updateByIdToId(id: Int, toId: Int, toName: String) = stage {
      val i = id // inorder to change the order of holes
      Queryable[Coffee1].filter(_.id == i)
    }.update(Coffee1(toId, toName))

    updateByIdToId(1368, cId, "AmirSh")

    val r4After = getAll()

    assertEquals("Updating a captured element with captured value", r1Before ++ List(Coffee1(cId, "AmirSh")), r4After)
    //    def updateByIdToSameId(id: Int, toName: String) = stage {
    //      val c = Coffee1(id, toName)
    //      Queryable[Coffee1].filter(_.id == id).update(c)
    //    }
    def updateByIdToSameId(id: Int, toName: String) = stage {
      Queryable[Coffee1].filter(_.id == id)
    }.update(Coffee1(id, toName))

    updateByIdToSameId(cId, "Amir")

    val r5After = getAll()

    assertEquals("Updating a captured element with captured value with repeated variable", r1Before ++ List(Coffee1(cId, "Amir")), r5After)

    def updateByIdToSameIdTuple2(id: Int, toName: String) = stage {
      Queryable[Coffee1].filter(_.id == id).map(x => (x.id, x.name))
    }.update((id, toName))

    updateByIdToSameId(cId, "AmirSh")

    val r6After = getAll()

    assertEquals("Updating a captured element with captured value with repeated variable", r1Before ++ List(Coffee1(cId, "AmirSh")), r6After)

    initAccountsTable()

    def getAccounts() = stage {
      Queryable[Account1]
    }.list()

    def getAccountsExcept(id: Int) = stage {
      Queryable[Account1].filter(_.id != id)
    }.list()

    val a1Before = getAccountsExcept(1)

    def updateAccount(id: Int, balance: Int, transfers: String) = {
      stage {
        //        Queryable[Account1].filter(_.id == id).update(Account1(id, balance, transfers))
        Queryable[Account1].filter(_.id == id)
      }.update(Account1(id, balance, transfers))
    }

    updateAccount(1, 1, ", 1")

    val a1After = getAccounts()

    assertEquals("Updating an element of account", a1Before ++ List(Account1(1, 1, ", 1")), a1After)

    DatabaseHandler.closeSession
  }

  @Test
  def queryTemplateTest {
    initCoffeeTable()
    import Shallow._
    import Shallow.TestH2._

    val inMem = List((1, "one"), (2, "two"), (3, "three"), (10, "ten"))

    for (threshold <- 1 to 10) {
      val r3 = stage {
        val q1 = Queryable[Coffee1] map (x => (x.id, if (x.id < threshold) "Low" else x.name))
        q1
      }.list()
      assertEquals("Template implicitly!", inMem.map(x => (x._1, if (x._1 < threshold) "Low" else x._2)), r3.toList)
    }

    //    val templ = templateMaker { (param: Long) =>
    //      (Queryable[Coffee1] map (x => (x.id, if (x.id < param) "Low" else x.name))).funcTemplate
    //    }
    //
    //    //    println(templ: Shallow.QueryTemplate[String, (Int, String)])
    //
    //    for (threshold <- 1 to 10) {
    //      val r3 = stage {
    //        templ(threshold)
    //      }
    //      assertEquals("Template by using templateMaker", inMem.map(x => (x._1, if (x._1 < threshold) "Low" else x._2)), r3.toList)
    //    }

    def template(threshold: Long) = stage {
      val q1 = Queryable[Coffee1] map (x => (x.id, if (x.id < threshold) "Low" else x.name))
      q1
    }.list()

    for (threshold <- 1 to 10) {
      val r3 = template(threshold)
      assertEquals("Template by invoking function", inMem.map(x => (x._1, if (x._1 < threshold) "Low" else x._2)), r3.toList)
    }
    def template2(lower: Long, upper: Long) = stage {
      val q1 = Queryable[Coffee1] map (x => (x.id, if (x.id < upper && x.id > lower) "Mid" else x.name))
      q1
    }.list()

    for (lower <- 1 to 10; upper <- (lower + 1) to 10) {
      val r3 = template2(lower, upper)
      assertEquals("Template with 2 args by invoking function", inMem.map(x => (x._1, if (x._1 < upper && x._1 > lower) "Mid" else x._2)), r3.toList)
    }

    def template2_r(lower: Long, upper: Long) = stage {
      val l = lower
      val u = upper
      val q1 = Queryable[Coffee1] map (x => (x.id, if (x.id < u && x.id > l) "Mid" else x.name))
      q1
      //      q1
    }.list()

    for (lower <- 1 to 10; upper <- (lower + 1) to 10) {
      val r3 = template2_r(lower, upper)
      assertEquals("Template with 2 reversed args by invoking function", inMem.map(x => (x._1, if (x._1 < upper && x._1 > lower) "Mid" else x._2)), r3.toList)
    }
    //    val r = (a: Int) => stage {
    //      a
    //    }
    //    val threshold = 1
    //    val r3 = stage {
    //      val q1 = Queryable[Coffee1] map (x => (x.id, if (x.id < threshold) "Low" else x.name))
    //      q1.toSeq
    //    }
    //    assertEquals("Query map (_1, _2) of Virtualized++ Table + if", inMem.map(x => (x._1, if (x._1 < threshold) "Low" else x._2)), r3.toList)

    def template_query(threshold: Long, string: String) = stage {
      Queryable[Coffee1] map (x => (x.id, if (x.id < threshold) string else x.name))
    }
    def inMem_template_query(threshold: Long, string: String) = 
      inMem.map(x => (x._1, if (x._1 < threshold) string else x._2))

    for (threshold <- 1 to 10) {
      val string = "LOW"
      val r3 = stage {
        template_query(threshold, string)
      }.list()
      assertEquals("Template by invoking function inside shadow block", inMem_template_query(threshold, string), r3.toList)
    }

    for (threshold <- 1 to 10) {
      val string = "low"
      val r3 = stage {
        template_query(threshold, string).map(x => (x._2 + "!", x._1 + 3))
      }.list()
      assertEquals("Template by invoking function inside shadow block composed with other expressions", inMem_template_query(threshold, string).map(x => (x._2 + "!", x._1 + 3)), r3.toList)
    }

    for (threshold <- 1 to 10) {
      val string = "low"
      val r3 = stage {
        template_query(threshold, string) zip template_query(threshold, "LOW")
      }.list()
      assertEquals("Template by invoking function inside shadow block composed with the same function", inMem_template_query(threshold, string) zip inMem_template_query(threshold, "LOW"), r3.toList)
    }

    def function_query(threshold: Long, string: String, query: Query[Coffee1]) = stage {
      query map (x => (x.id, if (x.id < threshold) string else x.name))
    }

    for (threshold <- 1 to 10) {
      val string = "LOW"
      val r3 = stage {
        function_query(threshold, string, Queryable[Coffee1])
      }.list()
      assertEquals("Template by invoking function with query parameter inside shadow block", inMem_template_query(threshold, string), r3.toList)
    }

    def function_zip_query(query1: Query[Coffee1], query2: Query[Coffee1]): JoinQuery[Coffee1, Coffee1] = stage {
      query1 zip query2
    }

    val zip_result = stage {
      function_zip_query(Queryable[Coffee1], Queryable[Coffee1]) map (x => ((x._1.id, x._1.name), (x._2.id, x._2.name)))
    }.list()

    assertEquals("Template by invoking function with 2 query parameters inside shadow block", inMem zip inMem, zip_result.toList)

    def function_zip_map_query(query1: Query[Coffee1], query2: Query[Coffee1]) = stage {
      query1 zip query2 map (x => ((x._1.id, x._1.name), (x._2.id, x._2.name)))
    }

    val zip_result_2 = stage {
      function_zip_map_query(Queryable[Coffee1], Queryable[Coffee1])
    }.list()

    assertEquals("Template by invoking function with 2 query parameters inside shadow block", inMem zip inMem  map (x => (x._1, x._2)), zip_result_2.toList)

    DatabaseHandler.closeSession
  }

  @Test
  def testUnion {
    {
      import DatabaseHandler.driver.simple._
      object Managers extends Table[(Int, String, String)]("managers") {
        def id = column[Int]("id")
        def name = column[String]("name")
        def department = column[String]("department")
        def * = id ~ name ~ department
      }

      object Employees extends Table[(Int, String, Int)]("employees") {
        def id = column[Int]("id")
        def name = column[String]("name2")
        def manager = column[Int]("manager")
        def * = id ~ name ~ manager

        // A convenience method for selecting employees by department
        def departmentIs(dept: String) = manager in Managers.where(_.department is dept).map(_.id)
      }

      implicit val session = DatabaseHandler.provideSession

      (Managers.ddl ++ Employees.ddl).create

      Managers.insertAll(
        (1, "Peter", "HR"),
        (2, "Amy", "IT"),
        (3, "Steve", "IT")
      )

      Employees.insertAll(
        (4, "Jennifer", 1),
        (5, "Tom", 1),
        (6, "Leonard", 2),
        (7, "Ben", 2),
        (8, "Greg", 3)
      )
    }
    import Shallow._
    import Shallow.TestH2._

    // testBasic
    val q1 = stage {
      for (m <- Queryable[Manager] filter { _.department == "IT" }) yield (m.id, m.name)
    }
    val q2 = stage {
      for (
        e <- Queryable[Employee] filter { e =>
          e.manager in {
            Queryable[Manager] filter { m =>
              m.department == "IT"
            } map (_.id)
          }
        }
      ) yield (e.id, e.name)
    }
    val q3 = stage {
      (q1 union q2).sortBy(_._2)
    }
    assertEquals(q3.list, List((2, "Amy"), (7, "Ben"), (8, "Greg"), (6, "Leonard"), (3, "Steve")))

    // testUnionWithoutProjection

    val q = stage {
      (Queryable[Manager] filter { _.name == "Peter" }) union (Queryable[Manager] filter { _.name == "Amy" })
    }
    assertEquals(Set(Manager(1, "Peter", "HR"), Manager(2, "Amy", "IT")), q.list.toSet)
    
    DatabaseHandler.closeSession
  }

  def initCoffeeTable() {
    import DatabaseHandler.driver.simple._

    object Coffee extends Table[(Int, String)]("COFFEE") {
      def id = column[Int]("ID")
      def name = column[String]("NAME")
      def * = id ~ name
    }

    object Test extends YYSlickCake {
      implicit val session = DatabaseHandler.provideSession

      (Coffee.ddl).create

      Coffee.insert((1, "one"))
      Coffee.insert((2, "two"))
      Coffee.insert((3, "three"))
      Coffee.insert((10, "ten"))
    }
    Test
  }

  def initSortTable() {
    import scala.slick.driver.H2Driver.simple._

    object Coffee extends Table[(Int, String)]("COFFEE") {
      def id = column[Int]("ID")
      def name = column[String]("NAME")
      def * = id ~ name
    }

    object Test extends YYSlickCake {
      implicit val session = DatabaseHandler.provideSession

      (Coffee.ddl).create

      Coffee.insert((2, "one"))
      Coffee.insert((1, "one"))
      Coffee.insert((3, "three"))
      Coffee.insert((10, "ten"))
    }
    Test
  }

  def initAccountsTable() {
    import scala.slick.shadow.VirtualizationModule._
    import scala.slick.driver.H2Driver.simple._
    implicit val session = DatabaseHandler.provideSession
    val numberOfAccounts = 2
    Account1Table.ddl.create
    for (i <- (0 until numberOfAccounts)) yield {
      Account1Table.insert(Account1(i, 0, " "))
      i
    }
  }

  def assertFail(f: => Unit) = {
    var succeeded = false
    try {
      f
      succeeded = true
    } catch {
      case e: Exception if !scala.util.control.Exception.shouldRethrow(e) =>
    }
    if (succeeded) fail("Exception expected")
  }

  val DatabaseHandler = Shallow.TestH2
}
