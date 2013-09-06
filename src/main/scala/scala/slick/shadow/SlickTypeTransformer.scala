package scala.slick.shadow

import scala.reflect.macros.Context
import scala.reflect.macros.Universe
import scala.reflect.runtime.universe.definitions.FunctionClass
import ch.epfl.yinyang.typetransformers.{ PolyTransformer }
import ch.epfl.yinyang.{ TransformationUtils }

class SlickTypeTransformer[C <: Context](ctx: C, override val debugLevel: Int = 0)/*(virtualTypes: List[Universe#Type])*/ extends PolyTransformer[C](ctx) with TransformationUtils {
  type Ctx = C
  import c.universe._

  // lazy val virtualTypeNames = virtualTypes.map(_.typeSymbol.name.toString())

  override def constructPolyTree(typeCtx: TypeContext, inType: Type): Tree = {
    log(s"handling $inType in $typeCtx")
    val res = typeCtx match {
      case TypeApplyCtx => inType match {
        case TypeRef(pre, sym, Nil) if !rewiredToThis(inType.typeSymbol.name.toString) =>
          TypeTree(inType)
        case TypeRef(pre, sym, args) if !isFunctionType(inType) && !args.isEmpty => {
          val typeArgs =
            args map { x => constructPolyTree(TypeApplyCtx, x) }
          AppliedTypeTree(Select(Ident(newTermName("scalaYY")), toType(sym)),
            typeArgs)
        }
        case _ => TypeTree(inType)
      }
      case OtherCtx => inType match {
        case TypeRef(pre, sym, Nil) if rewiredToThis(inType.typeSymbol.name.toString) =>
          super.constructPolyTree(typeCtx, inType)
        case TypeRef(pre, sym, Nil) =>
          super.constructPolyTree(typeCtx, inType)
        case TypeRef(pre, sym, args) if isFunctionType(inType) => {
          val argTrees = args map { x =>
            constructPolyTree(typeCtx, x)
          }
          AppliedTypeTree(Select(Ident(newTermName("scala")), toType(sym)),
            argTrees)
        }
        case TypeRef(pre, sym, args) if !isFunctionType(inType) => {
          val argsTrees =
            args map { x => constructPolyTree(TypeApplyCtx, x) }
          AppliedTypeTree(Select(This(newTypeName(className)), toType(sym)),
            argsTrees)
        }
        case _ => super.constructPolyTree(typeCtx, inType)
      }
    }
    log(s"res: $res")
    res
  }

}