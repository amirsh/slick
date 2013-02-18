package scala.slick.memory

import scala.language.implicitConversions
import slick.ast.{Type, Node, TypedType, BaseTypedType}
import scala.slick.compiler.QueryCompiler
import scala.slick.lifted._
import slick.profile._
import slick.lifted.ConstColumn
import slick.lifted.ColumnOrdered

/** A profile and driver for interpreted queries on top of the in-memory database. */
trait MemoryProfile extends RelationalProfile { driver: MemoryDriver =>

  type Backend = HeapBackend
  val backend: Backend = HeapBackend
  val compiler = QueryCompiler.relational

  override protected def computeCapabilities = super.computeCapabilities ++ MemoryProfile.capabilities.all
}

object MemoryProfile {
  object capabilities {
    /** Supports all MemoryProfile features which do not have separate capability values */
    val other = Capability("memory.other")

    /** All MemoryProfile capabilities */
    val all = Set(other)
  }
}

trait MemoryDriver extends RelationalDriver with MemoryProfile { driver =>

  /** The driver-specific representation of types */
  type TypeInfo = Type
  def typeInfoFor(t: Type) = t

  val profile: MemoryProfile = this
}

object MemoryDriver extends MemoryDriver
