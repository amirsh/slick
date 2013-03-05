package scala.slick.typeProviders

import java.sql.Connection
import _root_.scala.collection.mutable.ListMap
import _root_.scala.collection.mutable.ListBuffer
import _root_.scala.slick.driver.JdbcDriver
import _root_.scala.slick.jdbc.JdbcBackend
import _root_.scala.slick.jdbc.meta.MTable
import _root_.scala.slick.driver.HsqldbDriver
import _root_.scala.slick.driver.H2Driver

object Jdbc {
//  def list(conn: Connection, query: String): List[ListMap[String, Any]] = {
//    val stmt = conn.createStatement()
//    try {
//      val rs = stmt.executeQuery(query)
//      val buf = ListBuffer[ListMap[String, Any]]()
//      while (rs.next()) {
//        val record = ListMap[String, Any]()
//        for (i <- 1 to rs.getMetaData.getColumnCount) {
//          record += (rs.getMetaData.getColumnName(i).toLowerCase -> rs.getObject(i))
//        }
//        buf += record
//      }
//      buf.toList
//    } finally {
//      stmt.close()
//    }
//  }
//
//  def columns(conn: Connection)(tbl: String) =
//    list(conn, "show columns from " + tbl).map(row => {
//      val columnName = row("column_name").asInstanceOf[String]
//      val columnType = row("type").asInstanceOf[String]
//      Column(columnName, columnType)
//    })
//
//  def tables(conn: Connection) = {
//    val tableNames = list(conn, "show tables").map(_("table_name").asInstanceOf[String])
//    tableNames map (tbl => Schema(tbl, columns(conn)(tbl)))
//  }
  
  def tables(driver: JdbcDriver, db: JdbcBackend#DatabaseDef) = {
    import driver.simple.Database.threadLocalSession
//    val (cat: Option[String], schemaPattern: Option[String], namePattern: Option[String],
//    types: Option[Seq[String]]) = driver match {
//      case hsql: HsqldbDriver => (None, None, Some("%"), Some(Seq("TABLE")))
//      case h2: H2Driver => (Some(""), Some(""), None, None)
//      case _ => (None, None, None, None)
//    }
    db withSession {
//      val tables = MTable.getTables(cat, schemaPattern, namePattern, types).list
      val tables = driver.getTables.list
      tables map (t => {
        val columns = t.getColumns.list map (c => Column(c.column, driver.scalaTypeForColumn(c)))
        Schema(t.name.name, columns)
      }
      )
    }
  }
}

