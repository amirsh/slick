package scala.slick.typeProviders

case class Schema(table: String, columns: List[Column]){
  val scalaName = Naming.tableSQLToModule(table)
  val caseClassName = Naming.moduleToCaseClass(scalaName)
}

case class Column(name: String, scalaType: String) {
  val scalaName = Naming.columnSQLToField(name)

  override def toString = s"$name: $scalaType"
  
  
}

object Column {
 
}