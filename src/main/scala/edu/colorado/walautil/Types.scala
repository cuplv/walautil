package edu.colorado.walautil

import com.ibm.wala.ssa.ISSABasicBlock

object Types {
  /** type aliases */
  type MSet[T] = scala.collection.mutable.Set[T]
  type MMap[K,V] = scala.collection.mutable.Map[K,V]
  type MStack[T] = scala.collection.mutable.Stack[T]
  type CmpOp = com.ibm.wala.shrikeBT.IConditionalBranchInstruction.IOperator
  type BinOp = com.ibm.wala.shrikeBT.IBinaryOpInstruction.IOperator  
  type UnOp = com.ibm.wala.shrikeBT.IUnaryOpInstruction.IOperator
  type WalaBlock = ISSABasicBlock
}