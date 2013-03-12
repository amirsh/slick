package scala.slick.typeproviders

import scala.reflect.macros.Context
import java.sql._
import java.io.File
import scala.slick.driver.JdbcProfile
import scala.slick.driver.JdbcDriver
import scala.slick.jdbc.JdbcBackend
import com.typesafe.config.ConfigFactory
import scala.slick.SlickException
import com.typesafe.config.ConfigException
import scala.slick.schema.Retriever

object Macros {
  def DbImpl(c: Context)(url: c.Expr[String], configurationFileName: c.Expr[String]) = {
    import c.universe._
    import Flag._

    val macroHelper = new { val context: c.type = c } with MacroHelpers
    val runtimeMirror = scala.reflect.runtime.universe.runtimeMirror(Macros.this.getClass.getClassLoader)

    val (jdbcClass, urlConfig, slickDriverObject, userForConnection, passForConnection) = {
      val testDbs = "test-dbs/type-provider/conf/"
      val confFile = {
        try {
          val Expr(Literal(Constant(configFileName: String))) = configurationFileName
          val confFileName = if (configFileName.endsWith(".conf")) configFileName else configFileName + ".conf"
          val file = new File(confFileName)
          if (file.isFile() && file.exists())
            file
          else {
            val newFile = new File(testDbs + confFileName)
            if (newFile.isFile() && newFile.exists())
              newFile
            else
              throw new SlickException("Configuration file you provided does not exist")
          }
        } catch {
          case e: MatchError => throw new SlickException("You have to provide the config file name as literal", e)
        }
      }
      val conf = ConfigFactory.parseFile(confFile)
      @inline def c(key: String): String = try {
        conf.getString(key)
      } catch {
        case e: ConfigException.Missing => ""
        case e: ConfigException.WrongType => throw new SlickException(s"The value for $key should be String", e)
      }
      (c("jdbc-driver"), c("url"), c("slick-object"), c("username"), c("password"))
    }

    val connectionString: String = {
      val Expr(Literal(Constant(sUrl: String))) = url
      urlConfig + sUrl
    }

    def createConnection(): Connection = {
      val conString = connectionString
      Class.forName(jdbcClass)
      DriverManager.getConnection(connectionString, userForConnection, passForConnection)
    }

    def createDriver(): JdbcDriver = {
      val conString = connectionString
      val module = runtimeMirror.staticModule(slickDriverObject)
      val reflectedModule = runtimeMirror.reflectModule(module)
      val driver = reflectedModule.instance.asInstanceOf[JdbcDriver]
      driver
    }

    def generateCodeForTables(): List[Tree] = {
      val driver = createDriver()
      val db = driver.simple.Database.forURL(connectionString, driver = jdbcClass,
        user = userForConnection, password = passForConnection)
      val tables = Retriever.tables(driver, db)
      tables.flatMap(table => {
        // generate the dto case class
        val caseClass = macroHelper.tableToCaseClass(table)
        // generate the table object
        val tableModule = macroHelper.tableToModule(table, caseClass)

        List(caseClass, tableModule)
      })
    }

    val slickDriverTree = c.parse(slickDriverObject)
    val slickDriverExpr = c.Expr[JdbcDriver](slickDriverTree)
    val databaseTree = c.parse(slickDriverObject + ".simple.Database")
    val databaseExpr = c.Expr[JdbcBackend#DatabaseFactoryDef](databaseTree)
    val importTree = c.parse(s"import _root_.$slickDriverObject.simple._")
    val importExpr = c.Expr(importTree)

    val completeExpr = reify {
      class CONTAINER {
        importExpr.splice // import statement
        val driver = slickDriverExpr.splice
        val database = databaseExpr.splice.forURL(c.literal(connectionString).splice, driver = c.literal(jdbcClass).splice,
          user = c.literal(userForConnection).splice, password = c.literal(passForConnection).splice)
        // generated code will be spliced here
      }
    }
    val Expr(Block(List(ClassDef(_, containerType, _, Template(parents, self, body))), _)) = completeExpr

    val packageName = c.enclosingPackage.pid.toString
    val className = c.freshName(c.enclosingImpl.name).toTypeName

    c.introduceTopLevel(packageName, ClassDef(NoMods, className, Nil, Template(parents, self, body ++ generateCodeForTables())))
  }
}