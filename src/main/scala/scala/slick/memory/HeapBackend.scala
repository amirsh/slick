package scala.slick.memory

import scala.slick.backend.DatabaseComponent
import scala.slick.SlickException
import scala.slick.ast.FieldSymbol
import scala.collection.mutable.{ArrayBuffer, HashMap}

/** A simple database engine that stores data in heap data structures. */
trait HeapBackend extends DatabaseComponent {

  type Database = DatabaseDef
  type Session = SessionDef
  type DatabaseFactory = DatabaseFactoryDef

  val Database = new DatabaseFactoryDef
  val backend: HeapBackend = this

  class DatabaseDef extends super.DatabaseDef {
    protected val tables = new HashMap[String, HeapTable]
    def createSession(): Session = new SessionDef(this)
    def getTable(name: String): HeapTable = synchronized {
      tables.get(name).getOrElse(throw new SlickException(s"Table $name does not exist"))
    }
    def createTable(name: String, header: Header): HeapTable = synchronized {
      if(tables.contains(name)) throw new SlickException(s"Table $name already exists")
      val t = new HeapTable(name, header)
      tables += ((name, t))
      t
    }
    def dropTable(name: String): Unit = synchronized {
      if(!tables.remove(name).isDefined)
        throw new SlickException(s"Table $name does not exist")
    }
    def getTables: IndexedSeq[HeapTable] = synchronized {
      tables.values.toVector
    }
  }

  class DatabaseFactoryDef extends super.DatabaseFactoryDef {
    def apply(): Database = new DatabaseDef
  }

  class SessionDef(val database: Database) extends super.SessionDef {
    def close() {}

    def rollback() =
      throw new SlickException("HeapBackend does not currently support transactions")

    def withTransaction[T](f: => T) =
      throw new SlickException("HeapBackend does not currently support transactions")
  }

  type Row = IndexedSeq[Any]

  class Header(val fields: IndexedSeq[FieldSymbol])

  class HeapTable(val name: String, header: Header) {
    protected val data: ArrayBuffer[Row] = new ArrayBuffer[Row]
    def iterator: Iterator[Row] = data.iterator
    def append(row: Row): Unit = data.append(row)
  }
}

object HeapBackend extends HeapBackend {}
