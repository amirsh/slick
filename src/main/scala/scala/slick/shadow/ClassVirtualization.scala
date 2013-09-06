package scala.slick.shadow

import scala.slick.schema.Table
import scala.slick.schema.Column
import scala.slick.schema.QualifiedName
import scala.slick.typeproviders.MacroHelpers
import scala.slick.typeproviders.DefaultContextUtils
import scala.reflect.macros.{ Universe => MacroUniverse }
import scala.reflect.api.Universe
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.reflect.macros.Context

trait YYTransformers {
  val USE_VIRTUAL_CG = true
  val universe: Universe
  import universe._
  import Flag.CASE
  import Flag.PARAM
  val SYNTHETIC = scala.reflect.internal.Flags.SYNTHETIC.asInstanceOf[Long].asInstanceOf[FlagSet]
  val macroHelper = new {
    val universe: YYTransformers.this.universe.type = YYTransformers.this.universe
  } with MacroHelpers(DefaultContextUtils, "")

  var virtualTraverserIsApplied: Boolean = false
  var virtualSymbols: List[Symbol] = Nil
  var virtualMethodSymbols: List[MethodSymbol] = Nil

  object ClassVirtualization {
    import ch.epfl.yinyang.transformers.PostProcessing._
    def getStatementsFromTables[C <: Context](typeTransformer: SlickTypeTransformer[C]): List[(StatementContext, Tree)] = {
      val cv = new ClassVirtualization()
      val cvStatements = if (USE_VIRTUAL_CG) {
        val importStatement = (ClassContext, macroHelper.createImport(macroHelper.createObjectFromString("_root_.scala.slick.shadow.VirtualizationModule"), Nil))
        val tables = virtualSymbols map cv.getTableFromSymbol
        val caseAliasTerm = virtualSymbols zip tables map (x => cv.createCaseClassRepTerm(x._2)(x._1)) map (x => (ClassContext, x))
        val caseAliasType = virtualSymbols zip tables map (x => cv.createCaseClassRepType(x._2)(x._1)) map (x => (ClassContext, x))
        List(importStatement) ++ caseAliasTerm ++ caseAliasType
      } else
        cv.getStatementsFromTables(virtualSymbols)
      val mv = new MethodVirtualization(typeTransformer)
      val methodStatements = virtualMethodSymbols map mv.getMethodFromSymbol map (x => (ClassContext, x))
      val innerMethodStatements = virtualMethodSymbols map mv.getInnerMethod map (x => (MainContext, x))
      cvStatements ++ methodStatements ++ innerMethodStatements
    }
  }
  // workaround for compatibility of 2.10 and macro paradise
  object TermName {
    def apply(s: String): TermName = newTermName(s)
    def unapply(t: TermName): Option[String] = Some(t.toString)
  }
  object TypeName {
    def apply(s: String): TypeName = newTypeName(s)
    def unapply(t: TypeName): Option[String] = Some(t.toString)
  }

  class MethodVirtualization[C <: Context](val typeTransformer: SlickTypeTransformer[C]) {
    def transformType(tpe: Type): Tree = typeTransformer.transform(typeTransformer.OtherCtx, tpe.asInstanceOf[typeTransformer.c.universe.Type]).asInstanceOf[Tree]
    def getParamss(methodSymbol: MethodSymbol): List[List[ValDef]] = methodSymbol.paramss.map(_.map(p => {
      ValDef(Modifiers(PARAM | SYNTHETIC), TermName(p.name.toString), transformType(p.typeSignature), EmptyTree)
    }))
    def isParameterType(tpe: Type): Boolean = {
      def <<<[T: typeTransformer.c.TypeTag]: Boolean = tpe <:< typeTransformer.c.typeOf[T].asInstanceOf[Type]
      if (<<<[Int] || <<<[Long] || <<<[String] || <<<[Double] || <<<[Boolean])
        true
      else
        false
    }
    def getMethodFromSymbol(methodSymbol: MethodSymbol): Tree = {
      val paramss = getParamss(methodSymbol)
      // original(defaultValue[Int], defaultValue[String])
      val originalMethodTree = Apply(Ident(methodSymbol), methodSymbol.paramss.head.map({ p =>
        if (p.typeSignature <:< typeTransformer.c.typeOf[Shallow.Query[_]].asInstanceOf[Type])
          Select(Ident(TermName(p.name.toString)), TermName("getShallow"))
        else
          TypeApply(Ident(TermName("defaultValue")), List(TypeTree(p.typeSignature)))
      })) 
      // originalMethodTree.asInstanceOf[TransferQuery].underlying.transform
      val transMethod = Select(
        Select(
          TypeApply(
            Select(
              originalMethodTree, 
              TermName("asInstanceOf")
            ), 
            List(
              AppliedTypeTree(
                macroHelper.createClassFromString("_root_.scala.slick.shadow.lifting.TransferQuery"), 
                List(TypeTree(methodSymbol.returnType.asInstanceOf[TypeRef].args.head))
              )
            )
          ),                     
          TermName("underlying")
        ), 
        TermName("transform")
      )
      // transMethod(lift(2), lift("three"))
      val rhs = Apply(transMethod, methodSymbol.paramss.head.filter(p => isParameterType(p.typeSignature)).map({ p => 
        Ident(TermName(p.name.toString))
      }))
      DefDef(NoMods, TermName(virtualMethodName(methodSymbol.name.toString)), Nil, paramss, transformType(methodSymbol.returnType), rhs)
    }

    def virtualMethodName(methodName: String): String = "virtualised__" + methodName

    def getInnerMethod(methodSymbol: MethodSymbol): Tree = {
      val paramss = getParamss(methodSymbol)
      val rhs = 
        Apply(
          Ident(TermName(virtualMethodName(methodSymbol.name.toString))), 
          methodSymbol.paramss.head.map({ p => 
            Ident(TermName(p.name.toString))
          })
        )
      DefDef(NoMods, TermName(methodSymbol.name.toString), Nil, paramss, transformType(methodSymbol.returnType), rhs)
    }
  }

  class ClassVirtualization {
    def isCaseClassDef(tree: Tree): Boolean = tree match {
      case ClassDef(mods, _, _, _) if mods.hasFlag(CASE) => true
      case _ => false
    }
    def isCaseClassObject(tree: Tree): Boolean = tree match {
      case ModuleDef(mods, _, _) if mods.hasFlag(SYNTHETIC) => true
      case _ => false
    }
    def getTableFromSymbol(symbol: Symbol) = {
      def getNameOfSymbol(symbol: Symbol): Option[String] = {
        // workaround for SI-7424
        symbol.typeSignature
        symbol.annotations.foreach(_.tpe)
        symbol.annotations.headOption.map(_.scalaArgs.head).flatMap(annotationToName)
      }
      def annotationToName(tree: Tree): Option[String] = tree match {
        case Literal(Constant(name: String)) => Some(name)
        case _ => None
      }
      val tName = symbol.name.toString()
      val tableName = getNameOfSymbol(symbol).getOrElse(tName.toUpperCase())
      val tableQName = QualifiedName.tableName(tableName)
      val columns = symbol.typeSignature.member(nme.CONSTRUCTOR).typeSignature match {
        case MethodType(params, resultType) => params map { param =>
          val cName = param.name.toString()
          val columnName = getNameOfSymbol(param).getOrElse(cName.toUpperCase())
          val columnQName = QualifiedName.columnName(tableQName, columnName)
          val tpe = param.typeSignature
          Column(columnQName, tpe, cName, cName)
        }
      }
      Table(tableQName, columns, Nil, tName + "Table", tName + "Row")
    }
    def getTableFromCaseClassDef(classDef: ClassDef) = getTableFromSymbol(classDef.symbol)
    def getYYTableName(table: Table): String = "YY" + table.moduleName
    def createCaseClassRepTerm(table: Table)(symbol: Symbol): ValDef = {
      val mappedProjectionType = AppliedTypeTree(macroHelper.createClassFromString("_root_.scala.slick.lifted.MappedProjection"), List(Ident(newTypeName(table.caseClassName)), macroHelper.scalaProductClass))
      // module.*.asInstanceOf[MappedProjection].f
      val f = Select(TypeApply(Select(Select(Ident(newTermName(table.moduleName)), newTermName("$times")), newTermName("asInstanceOf")), List(mappedProjectionType)), newTermName("f"))
      // YYThisCase(f)
      val body = Apply(Select(macroHelper.createObjectFromString("_root_.scala.slick.shadow.deep.ThisCase"), newTermName("apply")), List(f))
      ValDef(NoMods, TermName(symbol.name.toString()), TypeTree(), body)
    }
    def createCaseClassRepType(table: Table)(symbol: Symbol): TypeDef = {
      val repTypeTree = macroHelper.createClassFromString("_root_.scala.slick.shadow.deep.YYRep")
      TypeDef(NoMods, TypeName(symbol.name.toString()), List(), AppliedTypeTree(repTypeTree, List(Ident(newTypeName(table.caseClassName)))))
    }
    def createYYTableClass(table: Table): ClassDef = {
      val yyTableName = getYYTableName(table)
      val PARAMACCESSOR = scala.reflect.internal.Flags.PARAMACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
      val typeParamName = TypeName(table.caseClassName)
      val yyTableType = macroHelper.createClassFromString("_root_.scala.slick.shadow.deep.YYTable")
      val tableSuper = AppliedTypeTree(yyTableType, List(Ident(typeParamName)))
      val tableParamVar = "table"
      val tableClassTree = macroHelper.createClass(table.moduleName, Nil)
      val tableParam = ValDef(Modifiers(PARAMACCESSOR), TermName(tableParamVar), tableClassTree, EmptyTree)
      val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())
      val constructorBody = ValDef(Modifiers(PARAM | PARAMACCESSOR), TermName(tableParamVar), tableClassTree, EmptyTree)
      val constructor = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List(constructorBody)), TypeTree(), Block(List(superCall), Literal(Constant(()))))
      val fields = table.columns map { c =>
        DefDef(NoMods, TermName(c.moduleFieldName), List(), List(), TypeTree(), Apply(Select(macroHelper.createObjectFromString("_root_.scala.slick.shadow.deep.YYColumn"), TermName("apply")), List(Select(Select(This(TypeName(yyTableName)), TermName(tableParamVar)), TermName(c.moduleFieldName)))))
      }
      val methods = tableParam :: constructor :: fields
      ClassDef(NoMods, TypeName(yyTableName), List(), Template(List(tableSuper), emptyValDef, methods))
    }
    def createYYTableModule(table: Table): ModuleDef = {
      val yyTableName = getYYTableName(table)
      val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List(Ident(TermName(table.moduleName))))
      val constructor = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(superCall), Literal(Constant(()))))
      ModuleDef(Modifiers(Flag.IMPLICIT), newTermName("implicit" + yyTableName), Template(List(Ident(TypeName(yyTableName))), emptyValDef, List(constructor)))
    }
    def createLiftedEmbeddingTableClass(table: Table): ClassDef = macroHelper.tableToModule(table) match {
      case ModuleDef(mods, TermName(name), Template(List(_), self, body)) => {
        val tableType = macroHelper.createClassFromString("_root_.scala.slick.driver.JdbcDriver.simple.Table")
        val typeParamName = TypeName(table.caseClassName)
        val tableSuper = AppliedTypeTree(tableType, List(Ident(typeParamName)))
        ClassDef(mods, TypeName(name), Nil, Template(List(tableSuper), self, body))
      }
    }
    def createLiftedEmbeddingTableModule(table: Table): ModuleDef = {
      val superCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())
      val constructor = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(superCall), Literal(Constant(()))))
      ModuleDef(NoMods, TermName(table.moduleName), Template(List(Ident(TypeName(table.moduleName))), emptyValDef, List(constructor)))
    }
    def createRepToTableImplicitDef(table: Table): DefDef = {
      val yyTableName = getYYTableName(table)
      //      val repTypeName = newTypeName("CakeRep")
      //      val repTypeTree = Ident(repTypeName)
      val repTypeTree = macroHelper.createClassFromString("_root_.scala.slick.shadow.deep.YYRep")
      val tableTypeName = newTypeName(yyTableName)
      val body = Apply(Select(New(Ident(tableTypeName)), nme.CONSTRUCTOR), List(TypeApply(Select(Select(Ident(newTermName("x")), newTermName("underlying")), newTermName("asInstanceOf")), List(Ident(newTypeName(table.moduleName))))))
      DefDef(Modifiers(Flag.IMPLICIT), newTermName("convImplicit" + yyTableName), List(), List(List(ValDef(Modifiers(PARAM), newTermName("x"), AppliedTypeTree(repTypeTree, List(Ident(newTypeName(table.caseClassName)))), EmptyTree))), Ident(tableTypeName), body)
    }
    def createTypeClassRow(table: Table)(symbol: Symbol): TypeDef = {
      val typeName = table.caseClassName
      val typeType = universe match {
        case mu: MacroUniverse => mu.TypeTree().setOriginal(Ident(symbol).asInstanceOf[mu.Tree]).asInstanceOf[Tree]
        case _ => macroHelper.createClassFromString(symbol.fullName)
      }
      TypeDef(NoMods, TypeName(typeName), List(), typeType)
    }
    def createValClassRow(table: Table)(symbol: Symbol): ValDef = {
      val termName = table.caseClassName
      val rhs = Ident(symbol.companionSymbol)
      ValDef(Modifiers(Flag.LOCAL), TermName(termName), TypeTree(), rhs)
    }
    def getTreesFromTable(table: Table)(symbol: Symbol): List[Tree] =
      List(createTypeClassRow(table)(symbol), createValClassRow(table)(symbol), createLiftedEmbeddingTableClass(table), createLiftedEmbeddingTableModule(table),
        createYYTableClass(table), createYYTableModule(table), createRepToTableImplicitDef(table))

    import ch.epfl.yinyang.transformers.PostProcessing._
    def getStatementsFromTables(symbols: List[Symbol]): List[(StatementContext, Tree)] = {
      symbols.flatMap(x => getTreesFromTable(getTableFromSymbol(x))(x).map(tree => (ClassContext, tree)))
    }
  }
  object VirtualClassCollector {
    def apply(tree: Tree): List[Symbol] = {
      val vcc = new VirtualClassCollector()
      vcc.traverse(tree)
      virtualTraverserIsApplied = true
      virtualSymbols = vcc.collected.toList
      virtualMethodSymbols = vcc.methods.toList
      virtualSymbols
    }
  }
  private final class VirtualClassCollector extends Traverser {
    private[YYTransformers] val collected = new HashSet[Symbol]()
    private[YYTransformers] val methods = new HashSet[MethodSymbol]()
    private val virtualTypes: List[Type] = {
      import universe.TypeTag._
      List(Boolean.tpe, Int.tpe, Long.tpe, Float.tpe, Double.tpe, typeOf[String].normalize)
    }
    private def isVirtual(tpe: Type): Boolean = {
      // normalize is used because of String
      virtualTypes.find(x => (tpe.normalize equals x)).isEmpty
    }
    override def traverse(tree: Tree) = tree match {
      // for collecting virtual types
      case typTree: TypTree if typTree.tpe != null => {
        def collectVirtuals(tpe: Type): Unit = tpe match {
          case t @ TypeRef(pre, sym, Nil) if isVirtual(t) => collected += t.typeSymbol
          case TypeRef(pre, sym, args) => args foreach collectVirtuals
          case _ => ()
        }
        collectVirtuals(typTree.tpe)
      }
      // for collecting methods
      case Apply(sel @ Ident(termName), args) if sel.symbol.isMethod => {
        // println("method: " + termName)
        methods += sel.symbol.asMethod
        args foreach traverse
      }
      case _ => super.traverse(tree)
    }
  }
}

final case class Entity(name: String) extends scala.annotation.StaticAnnotation
