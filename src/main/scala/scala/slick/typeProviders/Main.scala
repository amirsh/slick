package scala.slick.typeProviders

// Use H2Driver to connect to an H2 database
import scala.slick.driver.H2Driver.simple._
import scala.slick.driver.H2Driver.simple.{ Table => Tab }
import Database.threadLocalSession

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object Main1 {

  case class Supplier(id: Int, name: String, street: String, city: String, state: String, zip: String)

  // Definition of the SUPPLIERS table
  object Suppliers extends Tab[Supplier]("SUPPLIERS") {
    def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
    def name = column[String]("SUP_NAME")
    def street = column[String]("STREET")
    def city = column[String]("CITY")
    def state = column[String]("STATE")
    def zip = column[String]("ZIP")
    // Every table needs a * projection with the same type as the table's type parameter
    def * = id ~ name ~ street ~ city ~ state ~ zip <> (Supplier, Supplier.unapply _)
  }

  // Definition of the COFFEES table
  object Coffees extends Tab[(String, Int, Double, Int, Int)]("COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def supID = column[Int]("SUP_ID")
    def price = column[Double]("PRICE")
    def sales = column[Int]("SALES")
    def total = column[Int]("TOTAL")
    def * = name ~ supID ~ price ~ sales ~ total
    // A reified foreign key relation that can be navigated to create a join
    def supplier = foreignKey("SUP_FK", supID, Suppliers)(_.id)
  }

  val d1 = Database.forURL("jdbc:h2:coffees/coffees", driver = "org.h2.Driver", user = "sa", password = "")
  val d2 = Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver")
  val d3 = Database.forURL("jdbc:h2:coffees/supp", driver = "org.h2.Driver", user = "", password = "")

  // Connect to the database and execute the following block within a session
  d2 withSession {
    // The session is never named explicitly. It is bound to the current
    // thread as the threadLocalSession that we imported

    // Create the tables, including primary and foreign keys
    (Suppliers.ddl ++ Coffees.ddl).create

    // Insert some suppliers
    val s1 = Supplier(101, "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199")
    val s2 = Supplier(49, "Superior Coffee", "1 Party Place", "Mendocino", "CA", "95460")
    val s3 = Supplier(150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")
    Suppliers.insert(s1)
    Suppliers.insert(s2)
    Suppliers.insert(s3)

    println("Suppliers:")
    Query(Suppliers) foreach { sup =>
      println(sup)
    }
  }
  val conn = d3.createSession.conn
  println(Jdbc.tables(conn))
  println(Jdbc.list(conn, "show columns from " + "suppliers"))
}

object Main2 extends App {
  import scala.reflect.runtime.universe._
  
  val expr = reify {
	  import scala.reflect.runtime.universe._
	  
	  ;
  }
  val s = showRaw(expr)
  println(s)
  println(expr.actualType)
}

object OldTests1{
  import scala.reflect.runtime.universe._
  case class Supplier(id: Int, name: String, flag: Int)
  
  val expr = reify {

    // Definition of the SUPPLIERS table
    /*object Suppliers extends Tab[Supplier]("SUPPLIERS") {
      def id = column[Int]("SUP_ID")
      def name = column[String]("SUP_NAME")
      def flag = column[Int]("SUP_FLAG")
      def * = id ~ name ~ flag <> (Supplier, Supplier.unapply _)
//      def * = null
    }*/
    
    class A {
      val a = new A
      import a._
    }
  }
  val s = showRaw(expr)
  println(s)
  
  val n1 = This(newTypeName("a"))
  val n2 = This(newTypeName("b"))
  val s1 = Select(Select(n1, newTermName("c")), newTermName("d"))
  val s2 = Select(s1, newTermName("e"))
  val t = List(s1, s2)
//  val t3 = t.substituteThis(n1.symbol, n2)
  val t3 = new Transformer{
    override def transform(tree: Tree): Tree = {
//      tree
      tree match {
        case i@This(_) if i eq n1 => n2
        case _ => super.transform(tree)
      }
    }
  }.transformTrees(t)
  println(showRaw(t))
  println(showRaw(t3))
}