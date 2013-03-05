package scala.slick.typeProviders

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