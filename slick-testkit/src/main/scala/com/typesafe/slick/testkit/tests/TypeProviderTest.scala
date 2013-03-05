package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import scala.slick.ast.Dump
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

/*class TypeProviderTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._
// TODO
  def testTypeProvider {
//    case class Coffee(id: Int, name: String)
//    object Coffees extends Table[Coffee]("COFFEES") {
//      def id = column[Int]("ID")
//	  def name = column[String]("NAME")
//      def * = id ~ name <> (Coffee, Coffee.unapply _)
//    }
//    Coffees.ddl.create
//    Coffees.insertAll(Coffee(1, "n1"), Coffee(2, "n2"))

    import scala.slick.typeProviders._
    object Db1 extends TypeProvider.Db("test1")
	import Db1.driver.simple._
	import Database.threadLocalSession
	Db1.database.withSession {
	   val q1 = Query(Db1.Coffees.length)
	   assertEquals(2, q1.first)
	}
  }
}
*/