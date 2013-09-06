package scala.slick.shadow

import scala.slick.shadow.deep._

trait YYType {
  type CakeRep[T] = YYRep[T]
  type Column[T] = YYColumn[T]
  type Table[T] = YYTable[T]
  type Query[T] = YYQuery[T]
  type JoinQuery[T1, T2] = YYJoinQuery[T1, T2]
  type Int = YYColumn[scala.Int]
  type Long = YYColumn[scala.Long]
  type Double = YYColumn[scala.Double]
  type String = YYColumn[Predef.String]
  type Boolean = YYColumn[scala.Boolean]
  type ColumnOps[T] = YYColumn[T]
  type Ordering[T] = YYOrdering[T]
  type Option[T] = YYColumn[scala.Option[T]]
  type SingleColumnQuery[T] = YYSingleColumnQuery[T]
  class OptMaker[T](val value: YYColumn[T]) {
    def ? : YYOption[T] = YYOption.fromPlainColumn(value.underlying)
  }
}

trait YYConstantSourceType[T] {
  val defaultValue: T
}
trait YYConstantType[T, S] extends YYConstantSourceType[T]

object YYConstantType extends YYType {
  implicit object IntType extends YYConstantType[scala.Int, Int] {
    val defaultValue = 0
  }
  implicit object LongType extends YYConstantType[scala.Long, Long] {
    val defaultValue = 0L
  }
  implicit object DoubleType extends YYConstantType[scala.Double, Double] {
    val defaultValue = 0D
  }
  implicit object BooleanType extends YYConstantType[scala.Boolean, Boolean] {
    val defaultValue = false
  }
  implicit object StringType extends YYConstantType[Predef.String, String] {
    val defaultValue = ""
  }
}