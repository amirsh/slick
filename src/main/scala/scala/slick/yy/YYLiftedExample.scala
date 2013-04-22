package scala.slick.yy

import scala.slick.driver.H2Driver
import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession
import scala.slick.ast.Dump
import scala.slick.ast.Node
import scala.slick.ast.TableNode

object YYLiftedExample extends App {

  object TableA extends Table[(Int)]("TABLE_A") {
    def id = column[Int]("A_ID") 
    def * = id
  }

  Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
    (TableA.ddl).create
    
    TableA.insert(2)

    Dump(TableA)
    println(TableA.*.tpe)
    val q0 = Query(TableA)
//    val q0 = Query(TableA) map (a => a.id)
    val node = Node(q0)
    val tableNode = node.asInstanceOf[TableNode]
    println(node.nodeType)
    println(node.nodeIntrinsicSymbol)
    Dump(q0)
    Dump(tableNode.nodeTableProjection)
    println(tableNode.nodeTableProjection)
//    Dump(H2Driver.selectStatementCompiler.run(Node(q0)).tree)
    
    println(q0.first)
  }
}