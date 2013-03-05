package scala.slick.typeProviders

import scala.reflect.macros.Context

trait MacroHelpers{
	val context: Context
	import context.universe._
	import Flag._

	def columnToType(column: Column) = Ident(TypeName(column.scalaType))

	def createClass(className: String, packages: scala.List[String]) = {
		val classType = TypeName(className)
		val packagesType = packages map (TermName(_))
		if (packagesType.isEmpty)
			Ident(classType)
		else {
			val firstPackage = Ident(packagesType.head)
			val others = (packagesType.tail :+ classType)
			others.foldLeft[Tree](firstPackage)((prev, elem) => {
			  Select(prev, elem)
			})
		}
	}
	
	def tableToCaseClass(table: Schema) = {
      // load table schema
      val columns = table.columns
      val schema = columns map (column => (column.scalaName, columnToType(column)))
	  val caseClassName = table.caseClassName
	  val CASEACCESSOR = scala.reflect.internal.Flags.CASEACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
      val PARAMACCESSOR = scala.reflect.internal.Flags.PARAMACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
      val fields: List[Tree] = schema map { case (name, tpt) => ValDef(Modifiers(CASEACCESSOR | PARAMACCESSOR), name, tpt, EmptyTree) }
      val ctorParams: List[ValDef] = schema map { case (name, tpt) => ValDef(Modifiers(PARAM | PARAMACCESSOR), name, tpt, EmptyTree) }
      val ctor: Tree = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(ctorParams), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
      val caseClass = ClassDef(Modifiers(CASE), TypeName(caseClassName), Nil, Template(
      List(Select(Ident(TermName("scala")), TypeName("Product")), Select(Ident(TermName("scala")), TypeName("Serializable"))),
           emptyValDef,
           fields :+ ctor))
      caseClass
	}
	
	def columnToMethodDef(tableName: String)(column: Column) = {
	  DefDef(Modifiers(), TermName(column.scalaName), List(), List(), TypeTree(), Apply(TypeApply(Select(This(TypeName(tableName)), TermName("column")), List(columnToType(column))), List(Literal(Constant(column.name)))))
	}
	
	def tableToModule(table: Schema, caseClass: ClassDef) = {
	  val columns = table.columns
	  val tableName = table.scalaName
	  val tableType = createClass("Table", Nil)
      val Select(simplePackageType, _) = createClass("Table", List("scala", "slick", "driver", "H2Driver", "simple"))
      val tableSuper = AppliedTypeTree(tableType, List(Ident(caseClass.name)))
      val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List(Literal(Constant(table.table))))
      val constructor = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(superCall), Literal(Constant(()))))
      val fields = columns map columnToMethodDef(tableName)
	  val star = {
	    def getField(column: Column) = Select(This(TypeName(tableName)), TermName(column.scalaName))
	    val firstField = getField(columns.head)
	    val allFields = if (columns.length == 1) {
	      firstField
	    } else {
	      columns.tail.foldLeft[Tree](firstField)((prev, current) => {
	        val tilde = Select(prev, TermName("$tilde"))
	        Apply(tilde, List(getField(current)))
	      	}
	        )
	    }
	    val SYNTHETIC = scala.reflect.internal.Flags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet]
	    DefDef(Modifiers(), 
	        TermName("$times"), 
	        List(), 
	        List(), 
	        TypeTree(), 
	        Apply(
	            Select(allFields, TermName("$less$greater")), 
	            List(
	                Ident(caseClass.name.toTermName), 
	                Block(
	                    List(), 
	                    Function(
	                        List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName("x$0"), TypeTree(), EmptyTree)), 
	                        Apply(Select(Ident(caseClass.name.toTermName), TermName("unapply")), List(Ident(TermName("x$0")))))
	                )
	            )
	        )
	     )
	  }
      val methods = constructor :: (fields ++ (star :: Nil))
      val tableModule = ModuleDef(NoMods, TermName(tableName), Template(List(tableSuper), emptyValDef, methods))
      tableModule
	}
	  
}