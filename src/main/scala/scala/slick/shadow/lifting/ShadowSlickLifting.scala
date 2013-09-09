package scala.slick.shadow.lifting

import scala.slick.shadow.deep._
import ch.epfl.yinyang.api.{ BaseYinYangTypeTag, Interpreted, FullyStaged }
import scala.slick.shadow.{ Shallow => OShallow }
import scala.reflect.runtime.{ universe => ru }
import scala.slick.ast.QueryParameter
import scala.slick.jdbc.UnitInvoker
import scala.slick.shadow.ShadowInterpreter
import scala.slick.shadow.YYSlickCake
import scala.slick.shadow.YYConstantType

trait ShadowSlickLifting extends scala.slick.driver.JdbcDriver.ImplicitJdbcTypes with BaseYinYangTypeTag { self: YYSlickCake =>
  import scala.slick.ast.TypedType
  implicit object LiftUnit extends LiftEvidence[Unit, Unit] {
    def lift(v: Unit): Unit = v
    def hole(tpe: ru.TypeTag[Unit], symbolId: scala.Int): Unit = ()
  }
  implicit def LiftConst[T, S](implicit cstTpe: YYConstantType[T, S], ttag: ru.TypeTag[T], tpe: TypedType[T]): LiftEvidence[T, S] = new LiftEvidence[T, S] {
    def lift(v: T): S = YYConstColumn(v).asInstanceOf[S]
    def hole(tptag: ru.TypeTag[T], symbolId: scala.Int): S = {
      Hole[T](symbolId).asInstanceOf[S]
    }
  }
  implicit def liftQuery[T](implicit ttag: ru.TypeTag[OShallow.Query[T]]): LiftEvidence[OShallow.Query[T], Query[T]] = new LiftEvidence[OShallow.Query[T], Query[T]] {
    def lift(v: OShallow.Query[T]): Query[T] = v.asInstanceOf[TransferQuery[T]].underlying
    def hole(tpe: ru.TypeTag[OShallow.Query[T]], symbolId: scala.Int): Query[T] = ???
  }

  implicit def liftJoinQuery[T1, T2](implicit ttag: ru.TypeTag[OShallow.JoinQuery[T1, T2]]): LiftEvidence[OShallow.JoinQuery[T1, T2], JoinQuery[T1, T2]] = new LiftEvidence[OShallow.JoinQuery[T1, T2], JoinQuery[T1, T2]] {
    def lift(v: OShallow.JoinQuery[T1, T2]): JoinQuery[T1, T2] = v.asInstanceOf[TransferJoinQuery[T1, T2]].underlying.asInstanceOf[JoinQuery[T1, T2]]
    def hole(tpe: ru.TypeTag[OShallow.JoinQuery[T1, T2]], symbolId: scala.Int): JoinQuery[T1, T2] = ???
  }
}

class TransferQuery[T](val underlying: YYQuery[T], val cake: ShadowInterpreter, val params: IndexedSeq[Any]) extends OShallow.Query[T]
class TransferJoinQuery[T1, T2](override val underlying: YYQuery[(T1, T2)], override val cake: ShadowInterpreter, override val params: IndexedSeq[Any]) extends TransferQuery[(T1, T2)](underlying, cake, params) with OShallow.JoinQuery[T1, T2]