package scala.slick.typeProviders

import java.sql.Connection
import scala.collection.mutable.ListMap
import scala.collection.mutable.ListBuffer

object Jdbc {
  def list(conn: Connection, query: String): List[ListMap[String, Any]] = {
    val stmt = conn.createStatement()
    try {
      val rs = stmt.executeQuery(query)
      val buf = ListBuffer[ListMap[String, Any]]()
      while (rs.next()) {
        val record = ListMap[String, Any]()
        for (i <- 1 to rs.getMetaData.getColumnCount) {
          record += (rs.getMetaData.getColumnName(i).toLowerCase -> rs.getObject(i))
        }
        buf += record
      }
      buf.toList
    } finally {
      stmt.close()
    }
  }

  def columns(conn: Connection)(tbl: String) =
    list(conn, "show columns from " + tbl).map(row => {
      val columnName = row("column_name").asInstanceOf[String]
      val columnType = row("type").asInstanceOf[String]
      Column(columnName, columnType)
    })

  def tables(conn: Connection) = {
    val tableNames = list(conn, "show tables").map(_("table_name").asInstanceOf[String])
    tableNames map (tbl => Schema(tbl, columns(conn)(tbl)))
  }
}

