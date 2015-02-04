package edu.colorado.walautil

import scala.collection.JavaConversions._
import com.ibm.wala.classLoader.{IClass, IField, IMethod}
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.propagation.{AllocationSiteInNode, InstanceKey}
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.shrikeBT.IBinaryOpInstruction.Operator._
import com.ibm.wala.shrikeBT.IConditionalBranchInstruction.Operator._
import com.ibm.wala.shrikeBT.IShiftInstruction.Operator._
import com.ibm.wala.shrikeBT.IUnaryOpInstruction.Operator._
import com.ibm.wala.ssa._
import com.ibm.wala.types._

object ClassUtil {
  
  val THROWABLE = TypeReference.findOrCreate(ClassLoaderReference.Primordial, "Ljava/lang/Throwable")
  
  def isThrowableType(typ : TypeReference, cha : IClassHierarchy) : Boolean = 
    typ == THROWABLE || cha.computeSubClasses(THROWABLE).contains(typ) 

  def isLibrary(clazz : IClass) : Boolean =
    clazz.getClassLoader().getReference() == ClassLoaderReference.Primordial
      
  def isLibrary(method : IMethod) : Boolean = isLibrary(method.getDeclaringClass)
  
  def isLibrary(node : CGNode) : Boolean = isLibrary(node.getMethod())
  
  def isLibrary(fld : IField) : Boolean = isLibrary(fld.getDeclaringClass())
       
  def isLibraryToApplicationCall(caller : CGNode, callee : CGNode) : Boolean = isLibrary(caller) && !isLibrary(callee)

  def pretty(fld : IField) : String = fld.getName().toString()
  
  def pretty(typ : TypeReference) : String = typ.getName().toString()
  
  def pretty(clazz : IClass) : String = clazz.getName().toString() 
  
  def pretty(node : CGNode) : String = pretty(node.getMethod())
  
  // print method in class.methodName format
  def pretty(method : IMethod) : String = pretty(method.getReference())
        
  def pretty(method : MethodReference) : String = 
    method.getDeclaringClass().getName().toString() + "." + method.getSelector().getName().toString()
    
  def pretty(key : InstanceKey) : String = key match {
    case key : AllocationSiteInNode => pretty(key.getNode()) + "-" + key.getSite().toString()
    case _ => key.toString()
  }

  def isInnerClassThis(f : FieldReference) : Boolean = f.getName.toString.startsWith("this$")

  def isInnerClassThis(f : IField) : Boolean = isInnerClassThis(f.getReference)

  def isInnerOrEnum(c : IClass) : Boolean = c.getName().toString().contains('$')

  def isEnum(t : TypeReference, cha : IClassHierarchy) : Boolean = isEnum(cha.lookupClass(t), cha)

  def isEnum(c : IClass, cha : IClassHierarchy) : Boolean =
    cha.isSubclassOf(c, cha.lookupClass(TypeReference.JavaLangEnum))
  
  // TODO: Can we use WALA's StringStuff class to do some of these?
  // WALA uses leading L's in class names and /'s instead of .'s
  def walaifyClassName(className : String) : String = s"L$className".replace('.', '/')
  // strip off leading L and use /'s instead of .'s
  def deWalaifyClassName(typ : IClass) : String = deWalaifyClassName(typ.getReference())
  def deWalaifyClassName(typ : TypeReference) : String = deWalaifyClassName(typ.getName().toString())
  def deWalaifyClassName(typ : TypeName) : String = deWalaifyClassName(typ.toString())
  def stripWalaLeadingL(className : String) : String = {
    require(className.startsWith("L"), s"Bad class name $className does not begin with 'L'")    
    className.substring(1)
  }
  private def deWalaifyClassName(className : String) : String = {
    require(className.indexOf('.') < 0)
    stripWalaLeadingL(className).replace('/', '.').replace('$', '.')
  }
  def walaClassNameToPath(typ : TypeName) : String = stripWalaLeadingL(typ.toString())

  def partitionPackageAndName(className : String) : (String, String) = {
    val lastDot = className.lastIndexOf('.')
    if (lastDot >= 0) (className.substring(0, lastDot), className.substring(lastDot + 1))
    else {
      val DEFAULT_PACKAGE = ""
      (DEFAULT_PACKAGE, className)
    }
  }

  def makeTypeRef(typeName : String) : TypeReference =
    TypeReference.findOrCreate(ClassLoaderReference.Primordial, ClassUtil.walaifyClassName(typeName))

  /** @return all of the inner classes of @param c in @param cha */
  def getInnerClasses(c : IClass, cha : IClassHierarchy) : Iterable[IClass] = {
    val innerClassPrefix = c.getName.toString + "$"
    cha.filter(c => c.getName.toString.startsWith(innerClassPrefix))
  }

  // bytecodes expect a semicolon after a type; add it
  def typeRefToBytecodeType(typ : TypeReference) : String = s"${typ.getName().toString()};"
    
  def getNonReceiverParameterTypes(m : IMethod) : Iterable[TypeReference] = getParameterTypesInternal(m, 1)   
  def getParameterTypes(m : IMethod) : Iterable[TypeReference] = getParameterTypesInternal(m, 0)    
  private def getParameterTypesInternal(m : IMethod, firstParam : Int) : Iterable[TypeReference] = 
    (firstParam to m.getNumberOfParameters() - 1).map(i => m.getParameterType(i))
    
  // TODO: move this somewhere reasonable  
  def pp_instr (i : SSAInstruction, ir : IR) : Unit = if (ir != null) {
    val assign = " := "  
    val tbl = ir.getSymbolTable()
    i match {
        case i : SSANewInstruction =>
          print(v(i.getDef()) + assign + "new " + i.getConcreteType().getName())
          if (i.getConcreteType().isArrayType()) print("]") 
          print("@" + i.getNewSite().getProgramCounter()) 
          printParenthesizedUsesList(i)
        case i : SSAPutInstruction => 
          if (i.isStatic()) print(i.getDeclaredField().getDeclaringClass().getName())
          else print(v(i.getUse(0)))
          print("." + i.getDeclaredField().getName() + assign + v(i.getUse(1)))
        case i : SSAGetInstruction =>
          print(v(i.getDef()) + assign)
          if (i.isStatic()) print(i.getDeclaredField().getDeclaringClass().getName())
          else print(v(i.getUse(0)))
          print("." + i.getDeclaredField().getName())
        case i : SSAReturnInstruction =>
          print("return")
          if (!i.returnsVoid()) print(" " + v(i.getResult())) 
        case i : SSAAbstractInvokeInstruction =>
          if (i.hasDef()) print(v(i.getDef(0)) + assign)
          val startIndex = 
            if (i.isStatic()) { print(i.getDeclaredTarget().getDeclaringClass().getName()); 0 }
            else { print(v(i.getReceiver())); 1 }
          print("." + i.getDeclaredTarget().getName())
          printParenthesizedUsesList(i, startIndex)
        case i : SSAArrayLoadInstruction =>
          print(v(i.getDef(0)) + assign + v(i.getArrayRef()) + "[" + v(i.getIndex()) + "]")
        case i : SSAArrayStoreInstruction =>
          print(v(i.getArrayRef()) + "[" + v(i.getIndex()) + "]" + assign + v(i.getValue()))
        case i : SSAArrayLengthInstruction =>
          print(v(i.getDef()) + assign + v(i.getArrayRef()) + ".length")
        case i : SSAConditionalBranchInstruction =>
          print(v(i.getUse(0)))
          i.getOperator() match {
            case EQ => print(" == ")
            case NE => print(" != ")
            case LE => print(" <= ")
            case LT => print(" < ")
            case GE => print(" >= ")
            case GT => print(" > ")
            case _ => ()
          }
          print(v(i.getUse(1)))      
        case i : SSACheckCastInstruction =>
          print(v(i.getDef()) + assign + "(" + i.getDeclaredResultTypes().foldLeft ("") ((str, e) => str + e) + ") " + v(i.getUse(1)))
        case i : SSABinaryOpInstruction =>
          print(v(i.getDef()) + assign + v(i.getUse(0)))
          i.getOperator() match {
            case ADD => print(" + ")
            case SUB => print(" - ")
            case MUL => print(" * ")
            case DIV => print(" / ")
            case REM => print(" % ")
            case AND => print(" & ")
            case OR => print(" | ")
            case XOR => print(" ^ ")
            case SHL => print(" << ")
            case SHR => print(" >> ")
            case USHR => print(" >>> ")
            case op => print(op)
          }
          print(v(i.getUse(1)))
        case i : SSAPiInstruction =>
          print(v(i.getDef) + assign + " pi " + v(i.getVal) + " filter ")
          pp_instr(i.getCause, ir)
        case i : SSAUnaryOpInstruction =>
          print(v(i.getDef()) + assign)
          i.getOpcode() match {
            case NEG => print('!')
            case op => print(op)
          }
          print(v(i.getUse(0)))
        case i : SSAInstanceofInstruction => 
          print(v(i.getDef()) + assign + v(i.getRef()) + " instanceof " + i.getCheckedType())
        case i : SSAPhiInstruction =>
          print(v(i.getDef()) + assign + "phi")
          printParenthesizedUsesList(i)
        case _ => print(i)
    }
    
    def v(num : Int) : String =
     if (num <= 0) "TOP"
     else if (tbl != null && tbl.isConstant(num))
       if (tbl.isStringConstant(num)) "\"" + tbl.getConstantValue(num) + "\"" 
       else tbl.getConstantValue(num) + "" // using + "" instead of .toString() because the constant value can be null
     else "v" + num  
  
    def printParenthesizedUsesList(i : SSAInstruction, start : Int  = 0) = {
      require(start > -1)
      print('(')
      for (j <- start to i.getNumberOfUses() - 1) {
        print(v(i.getUse(j)))
        if (j != i.getNumberOfUses() - 1) print(", ")
      }
      print(')')
    }  
  }
}