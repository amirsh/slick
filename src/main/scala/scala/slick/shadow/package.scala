package scala.slick

import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers.PolyTransformer
import ch.epfl.yinyang.transformers.PostProcessing
import scala.language.experimental.macros
import scala.language.existentials
import scala.reflect.macros.Context
import scala.slick.jdbc.JdbcBackend
import scala.slick.driver.JdbcDriver

package object shadow {
  def stage[T](block: => T): T = macro implementations.stage[T]
  def stageDebug[T](block: => T): T = macro implementations.stageDebug[T]

  object implementations {
    val dslName = "scala.slick.shadow.ShadowInterpreter"
    def stage[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      //      println(c.universe.showRaw(block))
      val yyTranformers = new {
        val universe: c.universe.type = c.universe
        val mirror = c.mirror
      } with YYTransformers
      val className = YYTransformer.getClassName(dslName)
      val typeTransformer = new SlickTypeTransformer[c.type](c)
      typeTransformer.className = className
      val virtualSymbols = yyTranformers.VirtualClassCollector(block.tree)
      val virtualStatements = yyTranformers.ClassVirtualization.getStatementsFromTables(typeTransformer)

      val yyTransformer = YYTransformer[c.type, T](c)(dslName,
        typeTransformer,
        postProcessing = Some(new PostProcessing[c.type](c)(virtualStatements)),
        Map("shallow" -> false, "debug" -> 0, "featureAnalysing" -> false, "ascriptionTransforming" -> true, "liftTypes" -> List(c.typeOf[Shallow.Query[_]]), "assignedClassName" -> Some(className))
      )

      yyTransformer(block)
    }
    def stageDebug[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      //      println(c.universe.showRaw(block))
      val yyTranformers = new {
        val universe: c.universe.type = c.universe
      } with YYTransformers
      val className = YYTransformer.getClassName(dslName)
      val typeTransformer = new SlickTypeTransformer[c.type](c, 3)
      typeTransformer.className = className
      val virtualSymbols = yyTranformers.VirtualClassCollector(block.tree)
      val virtualStatements = yyTranformers.ClassVirtualization.getStatementsFromTables(typeTransformer)

      YYTransformer[c.type, T](c)(dslName,
        typeTransformer,
        postProcessing = Some(new PostProcessing[c.type](c)(virtualStatements)),
        Map("shallow" -> false, "debug" -> 3, "featureAnalysing" -> false, "ascriptionTransforming" -> true, "liftTypes" -> List(c.typeOf[Shallow.Query[_]]), "assignedClassName" -> Some(className))
      )(block)
    }

  }
}
