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

object Naming {
  def tableSQLToModule(tableSQL: String): String = {
    tableSQL.toLowerCase.capitalize
  }

  def moduleToCaseClass(module: String): String = {
    if (module.endsWith("s"))
      module.dropRight(1)
    else
      "C" + module
  }

  def columnSQLToField(columnSQL: String): String = {
    columnSQL.toLowerCase.foldLeft[(String, Boolean)](("", false))((prev, c) => {
      val (str, flag) = prev
      (c, flag) match {
        case ('_', _) => (str, true)
        case (_, true) => (str + Character.toUpperCase(c), false)
        case _ => (str + c, true)
      }
    })._1
  }
}

case class Schema(table: String, columns: List[Column]){
  val scalaName = Naming.tableSQLToModule(table)
  val caseClassName = Naming.moduleToCaseClass(scalaName)
}

case class Column(name: String, tpe: String) {
  val scalaType = tpe match {
    case s if s.startsWith("INTEGER") => "Int"
    case s if s.startsWith("VARCHAR") => "String"
    case s if s.startsWith("DOUBLE") => "Double"
    case _ => throw new Exception("Unknow type: " + tpe)
  }
  
  val scalaName = Naming.columnSQLToField(name)

  override def toString = s"$name: $scalaType"
}