package ca.uwaterloo.scalacg.analysis

import scala.collection.immutable.{ Set => ImmutableSet }
import scala.collection.mutable.Set
import ca.uwaterloo.scalacg.config.Global
import ca.uwaterloo.scalacg.util.Probe
import scala.runtime.ScalaRunTime

trait CallSites extends Global with Probe {
  import global._

  class AbstractCallSite(val receiverTree: Tree, val staticTarget: Symbol) {
    // The type of the receiver.
    lazy val receiver = receiverTree.tpe
    
    lazy val hasAbstractReceiver = receiver != null && receiver.typeSymbol.isAbstractType

    // Is this a constructor call?
    lazy val isConstructorCall = staticTarget.isConstructor

    // Is this a super call?
    lazy val isSuperCall = hasSuperReceiver || hasSuperAccessor

    lazy val isFunctionCall = receiver.isInstanceOf[MethodType]

    // The super receiver, if any.
    protected lazy val superReceiver = {
      receiverTree match {
        case s: Super => Some(s)
        case _ => None
      }
    }

    // Does this call site use "super" as a receiver?
    lazy val hasSuperReceiver = superReceiver.isDefined

    // The "this" receiver, if any.
    protected lazy val thisReceiver = {
      receiverTree match {
        case t: This => Some(t.symbol)
        case t => t.tpe match {
          case tpe: ThisType => Some(tpe.sym)
          case _ => None
        }
      }
    }

    // Does this call site use "this" as a receiver?
    lazy val hasThisReceiver = thisReceiver.isDefined

    // Does this call site us super in this fashion super[Z]?
    lazy val hasStaticSuperReference = superReceiver.isDefined && superReceiver.get.mix.nonEmpty

    // Get the named super (the Z in super[Z]) of this call site, if any
    lazy val staticSuperReference = {
      if (hasStaticSuperReference) {
        val superClass = receiver.baseClasses find (_.name.toTypeName == superReceiver.get.mix)
        if (superClass.isDefined) superClass.get.tpe else NoType
      } else NoType
    }

    // Does this call site has a "super" accessor (e.g., super$bar)?
    lazy val hasSuperAccessor = staticTarget.isSuperAccessor

    // Is the receiver of the call site a module class?
    lazy val hasModuleReceiver = receiver != null && receiver != NoType && receiver.typeSymbol.isModuleOrModuleClass
    //    lazy val hasModuleReceiver = receiverSymbol != null && receiverSymbol.isModuleOrModuleClass

    // Get the name of the static target method.
    lazy val targetName = {
      if (hasSuperAccessor) staticTarget.name drop nme.SUPER_PREFIX_STRING.length
      else staticTarget.name
    }

    override def toString = "<" + receiver + " :: " + signature(staticTarget) + ">"

    override def equals(other: Any): Boolean = other match {
      case that: AbstractCallSite => (that canEqual this) &&
        receiver == that.receiver &&
        staticTarget == that.staticTarget
      case _ => false
    }

    def canEqual(other: Any): Boolean = other.isInstanceOf[AbstractCallSite]

    override def hashCode: Int = 41 * (41 + (if (receiver != null) receiver.hashCode else 0)) + staticTarget.hashCode
  }

  class CallSite(receiverTree: Tree, override val staticTarget: Symbol,
    val enclMethod: Symbol, val position: Position, val annotations: ImmutableSet[String])
    extends AbstractCallSite(receiverTree, staticTarget) {

    // If the receiver is a This, then thisEnclMethod is the method whose implicit
    // this parameter is of the same class as the receiver This. Otherwise, thisEnclMethod
    // is NoSymbol.
    lazy val thisEnclMethod = {
      if (hasThisReceiver) {
        enclMethod.ownerChain.find { sym => sym.isMethod && sym.owner == thisReceiver.get }.getOrElse(NoSymbol)
      } else {
        NoSymbol
      }
    }
    
    lazy val csid = if(position.isDefined) receiver + "::" + signature(staticTarget) + "::" + signature(enclMethod) + "::" + 
    									   position.source.file.toString + "::" + position.line + "::" + position.column
    				else "unknown::-1::-1"

    override def toString = "<" + receiver + " :: " + signature(staticTarget) + " :: " + signature(enclMethod) + ">"

    override def equals(other: Any): Boolean = other match {
      case that: CallSite => (that canEqual this) &&
        receiver == that.receiver &&
        staticTarget == that.staticTarget &&
        enclMethod == that.enclMethod &&
        position == that.position
      case _ => false
    }

    override def canEqual(other: Any): Boolean = other.isInstanceOf[CallSite]

    override def hashCode: Int =
      41 * (
        41 * (
          super.hashCode) + enclMethod.hashCode) + position.hashCode

  }

  object CallSite {
    def apply(receiverTree: Tree, staticTarget: Symbol) =
      new AbstractCallSite(receiverTree, staticTarget)
    def apply(receiverTree: Tree, staticTarget: Symbol,
      enclMethod: Symbol, position: Position, annotations: ImmutableSet[String]) =
      new CallSite(receiverTree, staticTarget, enclMethod, position, annotations)
  }
}