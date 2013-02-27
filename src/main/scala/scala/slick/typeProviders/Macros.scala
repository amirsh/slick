package scala.slick.typeProviders

import scala.reflect.macros.Context
import language.experimental.macros
import java.sql._
import java.io.File

object Macros {
  def impl(c: Context)(url: c.Expr[String]) = {
    import c.universe._
    import Flag._

    def projectRoot = {
      val currentFile = c.enclosingUnit.source.file.path
      var projectRoot = new File(currentFile).getParent
      def coffeesDir = new File(projectRoot, "coffees")
      while (!coffeesDir.exists) {
        val projectRoot1 = new File(projectRoot).getParent
        if (projectRoot == projectRoot1) c.abort(c.enclosingPosition, "cannot detect SBT project root")
        projectRoot = projectRoot1
      }
      if (projectRoot == null) {
        projectRoot = raw"/Users/Amir/thesis/slick_paradise"
      }
      println(projectRoot)
      projectRoot
    }

    def connectionString(): String = {
      val Expr(Literal(Constant(sUrl: String))) = url
      val pathToDb = projectRoot + "/coffees/" + sUrl + ".h2.db"
      "jdbc:h2:" + pathToDb.stripSuffix(".h2.db")
    }
    
    val macroHelper = new {val context: c.type = c} with MacroHelpers
    
    def createConnection(): Connection = {
      val conString = connectionString()
      
      import scala.slick.driver.H2Driver.simple.Database
      Database.forURL(connectionString(), driver = "org.h2.Driver", user = userForConnection, password = "").createSession.conn
    }
    
    def userForConnection = if (connectionString.endsWith("coffees")) "sa" else ""

    def generateCodeForTables(): List[Tree] = {
      val conn = createConnection
      try {
        val tables = Jdbc.tables(conn)
      	tables.flatMap(table => {
          // generate the dto case class
          val caseClass = macroHelper.tableToCaseClass(table)
          // generate the table object
          val tableModule = macroHelper.tableToModule(table, caseClass)

          // that's it
          List(caseClass, tableModule)
        })
      } finally {
        conn.close()
      }
    }

    val Expr(Block(List(ClassDef(_, _, _, Template(parents, self, body))), _)) = reify {
      class CONTAINER {
        import scala.slick.driver.H2Driver.simple._
//        Class.forName("org.h2.Driver")
//        val conn = DriverManager.getConnection(c.literal(connectionString()).splice, c.literal(userForConnection).splice, "")
        val database = Database.forURL(c.literal(connectionString()).splice, driver = "org.h2.Driver", user = c.literal(userForConnection).splice, password = "")
        // generated code will be spliced here
      }
    }

    val packageName = c.enclosingPackage.pid.toString
    val className = c.freshName(c.enclosingImpl.name).toTypeName
    c.introduceTopLevel(packageName, ClassDef(NoMods, className, Nil, Template(parents, self, body ++ generateCodeForTables())))
    Select(c.enclosingPackage.pid, className)
  }

  type H2Db(url: String) = macro impl
}
class Macro(c: Context)
trait MacroHelpers{
	val context: Context
	import context.universe._
	import Flag._

//	val tableType = Select(Select(Select(Ident(TermName("scala")), TermName("slick")), TermName("typeProviders")),TypeName("Table"))
	
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
      val schema = columns map (column => (column.name, columnToType(column)))
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
//      val schema = columns map (column => (column.name, columnToType(column)))
	  val tableName = table.scalaName
      val tableType = createClass("Table", List("scala", "slick", "driver", "H2Driver", "simple"))
      val Select(simplePackageType, _) = tableType
      val tableSuper = AppliedTypeTree(tableType, List(Ident(caseClass.name)))
      val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List(Literal(Constant(table.table))))
      val constructor = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(superCall), Literal(Constant(()))))
//      val field = DefDef(Modifiers(), TermName("id"), List(), List(), TypeTree(), Apply(TypeApply(Select(This(TypeName(tableName)), TermName("column")), List(Ident(TypeName("Int")))), List(Literal(Constant("SUP_ID")))))
      val fields = columns map columnToMethodDef(tableName)
//      val star = DefDef(Modifiers(), TermName("$times"), List(), List(), TypeTree(), Literal(Constant(null)))
//	  val star = DefDef(Modifiers(), TermName("$times"), List(), List(), TypeTree(), Apply(Select(Apply(Select(Select(This(TypeName("Suppliers")), TermName("id")), TermName("$tilde")), List(Select(This(TypeName("Suppliers")), TermName("name")))), TermName("$less$greater")), List(Select(This(TypeName("Main2")), TermName("Supplier")), Block(List(), Function(List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName("x$0"), TypeTree(), EmptyTree)), Apply(Select(Select(This(TypeName("Main2")), TermName("Supplier")), TermName("unapply")), List(Ident(TermName("x$0")))))))))
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
//	    DefDef(Modifiers(), TermName("$times"), List(), List(), TypeTree(), Apply(Select(Apply(Select(Apply(Select(Select(This(TypeName("Suppliers")), TermName("id")), TermName("$tilde")), List(Select(This(TypeName("Suppliers")), TermName("name")))), TermName("$tilde")), List(Select(This(TypeName("Suppliers")), TermName("flag")))), TermName("$less$greater")), List(Select(This(TypeName("Main2")), TermName("Supplier")), Block(List(), Function(List(ValDef(Modifiers(PARAM | SYNTHETIC), TermName("x$0"), TypeTree(), EmptyTree)), Apply(Select(Select(This(TypeName("Main2")), TermName("Supplier")), TermName("unapply")), List(Ident(TermName("x$0")))))))))
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