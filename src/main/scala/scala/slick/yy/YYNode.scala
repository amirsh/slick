package scala.slick.yy

import scala.slick.ast.TypedType
import scala.slick.ast.TypedNode

abstract class YYNode[T : TypedType] extends TypedType[T] with TypedNode {
  final val tpe = implicitly[TypedType[T]]
}