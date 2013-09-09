package scala.slick.shadow.deep


import scala.slick.lifted.{ Column, Projection, Query, ColumnOrdered, Ordered => LOrdered, Shape, IdentityShape, ColumnExtensionMethods }
import scala.slick.lifted.{ PlainColumnExtensionMethods, OptionMapper2, ConstColumn, Rep, AbstractTable, NumericColumnExtensionMethods }
import scala.slick.lifted.{ NonWrappingQuery, WrappingQuery, BaseJoinQuery, CanBeQueryCondition, SingleColumnQueryExtensionMethods, StringColumnExtensionMethods }
import scala.slick.lifted.{ OptionColumnExtensionMethods, PlainColumnExtensionMethods, BooleanColumnExtensionMethods, FunctionSymbolExtensionMethods }
import FunctionSymbolExtensionMethods._
import scala.slick.ast.{ Node, Library, StaticType, LiteralNode, TypedType, Ordering => LOrdering, BaseTypedType, NumericTypedType }
import StaticType._
import scala.slick.jdbc.{ UnitInvoker, JdbcBackend }
import scala.slick.SlickException
import scala.slick.driver.{ JdbcProfile, JdbcDriver }

trait YYRep[T] {
  type NonRep = T
  def underlying: Rep[T]
}

trait YYValue[T] {
  def getValue: T
  def getValue(params: IndexedSeq[Any]): T
}

object YYValue {
  def fromLifted[T](rep: Any): YYRep[T] = rep match {
    case lifted: Rep[T] => applyUntyped(lifted)
    case product: Product => YYTuple.fromMixedTuple[Product](product).asInstanceOf[YYRep[T]]
  }
  def applyUntyped[T](rep: Rep[T]): YYRep[T] = rep match {
    case c: Column[_] => YYColumn(c)
    case t: AbstractTable[_] => YYTable(t)
    case q: Query[_, _] => YYQuery.fromQuery[Nothing](q.asInstanceOf[Query[Rep[Nothing], Nothing]]).asInstanceOf[YYRep[T]]
    case tup: Projection[_] => YYProjection.fromLiftedProjection(tup.asInstanceOf[Projection[Product]]).asInstanceOf[YYRep[T]]
  }
  def valueOfQuery[U, T <: Rep[U]](query: Query[T, U]): T = {
    import scala.language.reflectiveCalls
    type Unpackable = {
      val unpackable: scala.slick.lifted.ShapedValue[_, _]
    }
    val value = query.asInstanceOf[Unpackable].unpackable.value
    YYValue.fromLifted(value).underlying.asInstanceOf[T]
  }
  def apply[T](column: Column[T]): YYColumn[T] = YYColumn(column)
  def apply[T](table: AbstractTable[T]): YYTable[T] = YYTable(table)
  def apply[T, E <: YYRep[T]](rep: Rep[T]): E = YYValue.applyUntyped(rep).asInstanceOf[E]
}

class YYOption[T](val column: Column[Option[T]]) extends YYColumn[Option[T]] {
  override def underlying = column
  val optionColumnExtension = new OptionColumnExtensionMethods(column)
  def getOrElse[T1](default: => YYColumn[T]): YYColumn[T] = YYColumn(optionColumnExtension.getOrElse(default.getValue))
  def get: YYColumn[T] = YYColumn(optionColumnExtension.get)
}

object YYOption {
  def fromYYColumn[T](yyColumn: YYColumn[Option[T]]): YYOption[T] = new YYOption(yyColumn.underlying)
  def fromPlainColumn[T](column: Column[T]): YYOption[T] = new YYOption(new PlainColumnExtensionMethods(column).?)
}

trait YYColumn[T] extends ColumnExtensionOps[T] with ColumnNumericExtensionOps[T]
  with ColumnStringExtensionOps[T] with ColumnBooleanExtensionOps[T] with YYRep[T] with YYValue[T] {
  val column: Column[T]
  override def underlying = column
  def n = Node(column)
  implicit def om[T2, TR] = OptionMapper2.plain.asInstanceOf[OptionMapper2[T, T, TR, T, T2, TR]]
  def getValue: T = throw new SlickException("Accessing YYColumn value!")
  def getValue(params: IndexedSeq[Any]): T = throw new SlickException("Accessing YYColumn value!")
}

class YYOrdering[T](val ordering: LOrdering = LOrdering()) { self =>
  def reverse: YYOrdering[T] = new YYOrdering[T](ordering.reverse) {
    override def toOrdered(x: Rep[T]) = new LOrdered(self.toOrdered(x).columns.map {
      case (n, ord) => (n, ord.reverse)
    })
  }
  protected final def yyOrderingListToOrdered[U <: Product](x: Rep[U])(yyOrdProduct: Product): LOrdered =
    new LOrdered(YYOrdering.repToOrdered(x).columns.zip(yyOrdProduct.productIterator.toList.map(_.asInstanceOf[YYOrdering[_]])).map {
      case ((n, ord), yord) => (n, yord.ordering)
    })
  def toOrdered(x: Rep[T]): LOrdered = YYOrdering.repToOrdered(x)
}

object YYOrdering extends YYOrderingTuples {
  def apply[T](ord: YYOrdering[T]): YYOrdering[T] = ord
  def by[T, S](f: YYRep[T] => YYRep[S])(ord: YYOrdering[S]): YYOrdering[T] = new YYOrdering[T]() {
    override def toOrdered(x: Rep[T]): LOrdered = YYOrdering.repToOrdered(f(YYValue(x)).underlying)
  }
  def nonesFirst[T]: YYOrdering[Option[T]] = new YYOrdering[Option[T]](LOrdering().nullsFirst) {
    override def toOrdered(x: Rep[Option[T]]): LOrdered = x.asInstanceOf[Column[_]].asc.nullsFirst
  }
  def nonesLast[T]: YYOrdering[Option[T]] = new YYOrdering[Option[T]](LOrdering().nullsLast) {
    override def toOrdered(x: Rep[Option[T]]): LOrdered = x.asInstanceOf[Column[_]].asc.nullsLast
  }
  def nullsFirst[T]: YYOrdering[T] = nonesFirst[T].asInstanceOf[YYOrdering[T]]
  def nullsLast[T]: YYOrdering[T] = nonesLast[T].asInstanceOf[YYOrdering[T]]
  def repToOrdered[T](rep: Rep[T]): LOrdered = rep match {
    case column: Column[T] => column.asc
    case product: Product => new LOrdered(product.productIterator.flatMap { x: Any =>
      repToOrdered(x.asInstanceOf[Rep[_]]).columns
    }.toSeq)
  }
  def fromOrdering[T](ord: Ordering[T]): YYOrdering[T] = new YYOrdering[T]()
  val String = fromOrdering(Ordering.String)
  val Int = fromOrdering(Ordering.Int)
  val Long = fromOrdering(Ordering.Long)
  val Double = fromOrdering(Ordering.Double)
}

object YYColumn {
  def apply[T](c: Column[T]): YYColumn[T] = c.tpe match {
    case o: scala.slick.ast.OptionType => new YYOption[Nothing](c.asInstanceOf[Column[Option[Nothing]]]).asInstanceOf[YYColumn[T]]
    case _ => new YYColumn[T] {
      val column = c
    }
  }
}

trait YYTable[T] extends YYRep[T] {
  val table: AbstractTable[T]
  override def underlying = table
}

object YYTable {
  def apply[T](t: AbstractTable[T]): YYTable[T] = new YYTable[T] {
    val table = t
  }
}

class YYConstColumn[T](val constColumn: ConstColumn[T]) extends YYColumn[T] {
  override val column = constColumn
  override def getValue: T = constColumn.value
  override def getValue(params: IndexedSeq[Any]): T = getValue
}

object YYConstColumn {
  def apply[T: TypedType](v: T): YYColumn[T] = new YYConstColumn(ConstColumn[T](v))
}

trait ColumnExtensionOps[T] { self: YYColumn[T] =>
  def extendedColumn = new PlainColumnExtensionMethods(column)
  def isNull: YYColumn[Boolean] = YYColumn(extendedColumn.isNull)
  def isNotNull: YYColumn[Boolean] = YYColumn(extendedColumn.isNotNull)
  def is[T2](e: YYColumn[T2]): YYColumn[Boolean] = YYColumn(extendedColumn is e.column)
  def ===[T2](e: YYColumn[T2]): YYColumn[Boolean] = YYColumn(extendedColumn === e.column)
  def __==[T2](e: YYColumn[T2]): YYColumn[Boolean] = YYColumn(extendedColumn === e.column)
  def __!=[T2](e: YYColumn[T2]): YYColumn[Boolean] = YYColumn(extendedColumn =!= e.column)
  def >[T2](e: YYColumn[T2]): YYColumn[Boolean] = YYColumn(extendedColumn > e.column)
  def <[T2](e: YYColumn[T2]): YYColumn[Boolean] = YYColumn(extendedColumn < e.column)
  def in(q: YYQuery[T]): YYColumn[Boolean] = YYColumn(extendedColumn in (q.query.asInstanceOf[Query[Column[T], _]]))
}

trait ColumnNumericExtensionOps[T] { self: YYColumn[T] =>
  lazy val numericColumn = new NumericColumnExtensionMethods[T, T](column)
  def +(e: YYColumn[T]): YYColumn[T] = e.column.tpe match {
    case stringTpe: JdbcDriver.columnTypes.StringJdbcType => (this ++ e.asInstanceOf[YYColumn[String]]).asInstanceOf[YYColumn[T]]
    case numericTpe: NumericTypedType => YYColumn(numericColumn + e.column)
  }
  def -(e: YYColumn[T]): YYColumn[T] = YYColumn(numericColumn - e.column)
  def *(e: YYColumn[T]): YYColumn[T] = YYColumn(numericColumn * e.column)
  def /(e: YYColumn[T]): YYColumn[T] = YYColumn(numericColumn / e.column)
  def %(e: YYColumn[T]): YYColumn[T] = YYColumn(numericColumn % e.column)
  def abs = YYColumn(numericColumn.abs)
  def ceil = YYColumn(numericColumn.ceil)
  def floor = YYColumn(numericColumn.floor)
  def sign = YYColumn(numericColumn.sign)
  def toDegrees = YYColumn(numericColumn.toDegrees)
  def toRadians = YYColumn(numericColumn.toRadians)
}

trait ColumnStringExtensionOps[T] { self: YYColumn[T] =>
  lazy val stringColumn = new StringColumnExtensionMethods[String](column.asInstanceOf[Column[String]])
  def length() = YYColumn(stringColumn.length)
  def like(e: YYColumn[String], esc: Char = '\0') = YYColumn(stringColumn.like(e.column, esc))
  def ++(e: YYColumn[String]) = YYColumn(stringColumn ++ e.column)
  def startsWith(s: YYColumn[String]) = YYColumn(stringColumn.startsWith(s.getValue))
  def endsWith(s: YYColumn[String]) = YYColumn(stringColumn.endsWith(s.getValue))
  def toUpperCase() = YYColumn(stringColumn.toUpperCase)
  def toLowerCase() = YYColumn(stringColumn.toLowerCase)
  def ltrim() = YYColumn(stringColumn.ltrim)
  def rtrim() = YYColumn(stringColumn.rtrim)
  def trim() = YYColumn(stringColumn.trim)
}

trait ColumnBooleanExtensionOps[T] { self: YYColumn[T] =>
  lazy val booleanColumn = new BooleanColumnExtensionMethods[Boolean](column.asInstanceOf[Column[Boolean]])
  def &&(b: YYColumn[Boolean]) = YYColumn(booleanColumn && b.column)
  def ||(b: YYColumn[Boolean]) = YYColumn(booleanColumn || b.column)
  def unary_! = YYColumn(!booleanColumn)
}

object YYShape {
  def ident[U]: Shape[Rep[U], U, Rep[U]] = Shape.impureShape.asInstanceOf[Shape[Rep[U], U, Rep[U]]]
  def apply[U] = ident[U]
}

trait YYQuery[U] extends QueryOps[U] with YYRep[Seq[U]] {
  val query: Query[Rep[U], U]
  def repValue: Rep[U] = YYValue.valueOfQuery(query)
  override def underlying = query
  object BooleanRepCanBeQueryCondition extends CanBeQueryCondition[Rep[Boolean]] {
    def apply(value: Rep[Boolean]) = value.asInstanceOf[Column[Boolean]]
  }
  def transform(varargs: YYColumn[_]*): YYQuery[U] = {
    val nodes = varargs.toIndexedSeq
    import scala.slick.compiler.Transformer
    import scala.slick.ast.QueryParameter
    val transformer = new Transformer { 
      override val keepType = true
      override def replace: PartialFunction[Node, Node] = new PartialFunction[Node, Node] {
        def apply(n: Node) = n match {
          case param: QueryParameter with IndexedParameter => nodes(param.index).underlying.nodeDelegate
          case _ => throw new SlickException("Incorrect parameter")
        }
        def isDefinedAt(n: Node) = n match {
          case param: QueryParameter => true
          case _ => false
        }
      }
    }
    val queryNode = query match {
      case wq: WrappingQuery[_, _] => wq.nodeDelegate
      case nwq: NonWrappingQuery[_, _] => nwq.nodeDelegate
    }
    val transformedNode = transformer.once(queryNode)
    query match {
      // doesn't work, 'pattern type is incompatible with expected type'
      // case bjq: BaseJoinQuery[_, _, _, _] => {
      //   type U1 = Any
      //   type U2 = Any
      //   type Q = Query[(Rep[U1], Rep[U2]), (U1, U2)]
      //   YYQuery.fromJoinQuery(new WrappingQuery[Rep[U], U](transformedNode, bjq.base).asInstanceOf[Q]).asInstanceOf[YYQuery[U]]
      // }
      case wq: WrappingQuery[_, _] => 
        if (wq.isInstanceOf[BaseJoinQuery[_, _, _, _]]) {
          type U1 = Any
          type U2 = Any
          type Q = Query[(Rep[U1], Rep[U2]), (U1, U2)]
          YYQuery.fromJoinQuery(new WrappingQuery[Rep[U], U](transformedNode, wq.base).asInstanceOf[Q]).asInstanceOf[YYQuery[U]]
        }
        else
          YYQuery.fromQuery(new WrappingQuery[Rep[U], U](transformedNode, wq.base))
      case nwq: NonWrappingQuery[_, _] => YYQuery.fromQuery(new NonWrappingQuery[Rep[U], U](transformedNode, nwq.unpackable))
    }
    // YYQuery.fromQuery(query match {
    //   case wq: WrappingQuery[_, _] => new WrappingQuery[Rep[U], U](transformedNode, wq.base)
    //   case nwq: NonWrappingQuery[_, _] => new NonWrappingQuery[Rep[U], U](transformedNode, nwq.unpackable)
    // })
  }

  def getShallow: scala.slick.shadow.Shallow.Query[U] = new scala.slick.shadow.lifting.TransferQuery[U](this, null, null)
}

trait YYJoinQuery[U1, U2] extends YYQuery[(U1, U2)] {
  def on(pred: (YYRep[U1], YYRep[U2]) => YYColumn[Boolean]): YYQuery[(U1, U2)] = {
    def underlyingPredicate(_1: Rep[U1], _2: Rep[U2]): Column[Boolean] =
      pred(YYValue.applyUntyped(_1), YYValue.applyUntyped(_2)).underlying
    YYQuery.fromJoinQuery(query.asInstanceOf[BaseJoinQuery[Rep[U1], Rep[U2], U1, U2]].on[Column[Boolean]](underlyingPredicate _))
  }
}

object YYQuery {
  def create[U](q: Query[Rep[U], U], e: Rep[U]): YYQuery[U] = new YYQuery[U] {
    val query = q
    override def repValue: Rep[U] = e
  }
  def fromQuery[U](q: Query[Rep[U], U]): YYQuery[U] = create(q, YYValue.valueOfQuery(q))
  def fromJoinQuery[U1, U2](q: Query[(Rep[U1], Rep[U2]), (U1, U2)]): YYJoinQuery[U1, U2] = new YYJoinQuery[U1, U2] {
    val query = q.asInstanceOf[Query[(Rep[(U1, U2)]), (U1, U2)]]
  }
  def apply[U](v: YYRep[U]): YYQuery[U] = create(Query(v.underlying)(YYShape.ident[U]), v.underlying)
  def apiApply[U <: YYRep[_]](v: U): YYQuery[v.NonRep] = apply(v.asInstanceOf[YYRep[v.NonRep]])
}

trait QueryOps[T] { self: YYQuery[T] =>
  protected def underlyingProjection[S](projection: YYRep[T] => YYRep[S]): Rep[T] => Rep[S] =
    (x: Any) => projection(YYValue.fromLifted[T](x)).underlying
  def map[S](projection: YYRep[T] => YYRep[S]): YYQuery[S] = YYQuery.fromQuery(query.map(underlyingProjection(projection))(YYShape.ident[S]))
  def filter(projection: YYRep[T] => YYRep[Boolean]): YYQuery[T] = YYQuery.fromQuery(query.filter(underlyingProjection(projection))(BooleanRepCanBeQueryCondition))
  def withFilter(projection: YYRep[T] => YYRep[Boolean]): YYQuery[T] = filter(projection)
  def flatMap[S](projection: YYRep[T] => YYQuery[S]): YYQuery[S] = YYQuery.fromQuery(query flatMap { (x: Rep[T]) =>
    projection(YYValue(x)).query
  })
  def sortBy[S](f: YYRep[T] => YYRep[S])(ord: YYOrdering[S]): YYQuery[T] = YYQuery.fromQuery(query.sortBy(underlyingProjection(f))((x: Rep[S]) => ord.toOrdered(x)))
  def sorted(ord: YYOrdering[T]): YYQuery[T] = YYQuery.fromQuery(query.sorted((x: Rep[T]) => ord.toOrdered(x)))
  def take(i: YYColumn[Int]): YYQuery[T] = YYQuery.fromQuery(query.take(i.getValue))
  def drop(i: YYColumn[Int]): YYQuery[T] = YYQuery.fromQuery(query.drop(i.getValue))
  def length: YYColumn[Int] = YYColumn(query.length)
  def groupBy[S](f: YYRep[T] => YYRep[S]): YYQuery[(S, scala.slick.shadow.Shallow.Query[T])] =
    YYQuery.fromQuery(query.groupBy(underlyingProjection(f))(YYShape[S], YYShape[T]).asInstanceOf[Query[Rep[(S, scala.slick.shadow.Shallow.Query[T])], (S, scala.slick.shadow.Shallow.Query[T])]])
  def union(q2: YYQuery[T]): YYQuery[T] = YYQuery.fromQuery(query.union(q2.query))
  def innerJoin[S](q2: YYQuery[S]): YYJoinQuery[T, S] = YYQuery.fromJoinQuery(query.innerJoin(q2.query))
  def leftJoin[S](q2: YYQuery[S]): YYJoinQuery[T, S] = YYQuery.fromJoinQuery(query.leftJoin(q2.query))
  def rightJoin[S](q2: YYQuery[S]): YYJoinQuery[T, S] = YYQuery.fromJoinQuery(query.rightJoin(q2.query))
  def outerJoin[S](q2: YYQuery[S]): YYJoinQuery[T, S] = YYQuery.fromJoinQuery(query.outerJoin(q2.query))
  def zip[S](q2: YYQuery[S]): YYJoinQuery[T, S] = YYQuery.fromJoinQuery(query.zip(q2.query))
  def zipWithIndex: YYJoinQuery[T, Long] = YYQuery.fromJoinQuery(query.zipWithIndex)
}

final class YYSingleColumnQuery[T](val q: YYQuery[T]) extends AnyVal {
  @inline def singleColumnQuery = new SingleColumnQueryExtensionMethods[T, T](q.underlying.asInstanceOf[Query[Column[T], T]])
  type OptionTM = TypedType[Option[T]]
  def min(implicit tm: OptionTM): YYOption[T] = new YYOption(singleColumnQuery.min)
  def max(implicit tm: OptionTM): YYOption[T] = new YYOption(singleColumnQuery.max)
  def avg(implicit tm: OptionTM): YYOption[T] = new YYOption(singleColumnQuery.avg)
  def sum(implicit tm: OptionTM): YYOption[T] = new YYOption(singleColumnQuery.sum)
}
