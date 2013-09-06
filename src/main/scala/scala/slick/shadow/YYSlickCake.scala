package scala.slick.shadow

import scala.language.implicitConversions
import scala.slick.lifted.{ Case, AbstractTable }
import scala.slick.ast.BaseTypedType
import scala.slick.jdbc.JdbcBackend
import scala.slick.SlickException
import scala.slick.profile.BasicDriver
import scala.slick.driver.{ H2Driver, JdbcDriver, JdbcProfile }
import scala.slick.shadow.deep._
import ch.epfl.yinyang.api.VirtualAny

trait YYSlickCake extends YYType with YYSlickCakeTuples with VirtualAny {
  val Ordering = YYOrdering
  val String = YYOrdering.String
  val Int = YYOrdering.Int
  val Long = YYOrdering.Long
  val Double = YYOrdering.Double
  implicit def fixClosureContraVariance[T, U <: YYRep[T], S](x: U => S) = x.asInstanceOf[YYRep[T] => S]
  implicit def yyRepIntToColumnOps(x: CakeRep[scala.Int]): Int = x.asInstanceOf[Int]
  implicit def yyRepLongToColumnOps(x: CakeRep[scala.Long]): Long = x.asInstanceOf[Long]
  implicit def yyRepStringToColumnOps(x: CakeRep[Predef.String]): String = x.asInstanceOf[String]
  implicit def yyRepDoubleToColumnOps(x: CakeRep[scala.Double]): Double = x.asInstanceOf[Double]
  implicit def yyRepBooleanToColumnOps(x: CakeRep[scala.Boolean]): Boolean = x.asInstanceOf[Boolean]
  implicit def yyRepSQueryToQuery[T](x: CakeRep[scalaYY.Query[T]]): Query[T] = x.asInstanceOf[Query[T]]
  implicit def yyColumnOptionToYYOption[T](x: YYColumn[scala.Option[T]]): YYOption[T] = YYOption.fromYYColumn(x)
  object Queryable {
    def apply[T](implicit t: YYTable[T]): Query[T] = YYQuery.apply(t)
  }
  object Query {
    def apply[T](v: YYRep[T]): YYQuery[T] = YYQuery.apply(v)
    def ofTable[T](t: YYTable[T]): YYQuery[T] = YYQuery.apply(t)
  }
  object Shallow {
    def ColumnOps[T](value: YYColumn[T]): YYColumn[T] = value
    def intWrapper(value: Int): Int = value
    def stringWrapper(value: String): String = value
    def OptMaker[T](value: YYColumn[T]): OptMaker[T] = new OptMaker(value)
    def SingleColumnQuery[T](value: YYQuery[T]): YYSingleColumnQuery[T] = new YYSingleColumnQuery(value)
    def nonesFirst[T]: YYOrdering[scala.Option[T]] = YYOrdering.nonesFirst
    def nonesLast[T]: YYOrdering[scala.Option[T]] = YYOrdering.nonesLast
    def nullsFirst[T]: YYOrdering[T] = YYOrdering.nullsFirst
    def nullsLast[T]: YYOrdering[T] = YYOrdering.nullsLast
    def queryToShadowExecutor[T](query: YYQuery[T]): DummyExecutor[T] = throw new SlickException("You cannot invoke inside the block")
  }
  class DummyExecutor[T] {
    def list()(): List[T] = ???
    def first()(): T = ???
    def insert(value: T)(): Int = ???
    def update(value: T)(): Int = ???
  }
  object scalaYY extends scalaYYTuples {
    type Query[T] = scala.slick.shadow.Shallow.Query[T]
    type Option[T] = scala.Option[T]
  }
  def __ifThenElse[T: BaseTypedType](c: => Boolean, t: Column[T], e: Column[T]): Column[T] =
    YYColumn(Case.If(c.underlying) Then (t.underlying) Else (e.underlying))
  def __equals[T](t: Column[T], e: Column[T]) = t === e
  object Table {
    def getTable[S](implicit mapping: Table[S]): Table[S] = mapping
  }

  def defaultValue[T](implicit tpe: YYConstantType[T, _]): T = {
    tpe.defaultValue
  }
}