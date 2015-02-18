package edu.colorado.walautil

import com.ibm.wala.util.graph.impl.BasicNodeManager
import com.ibm.wala.util.graph.{AbstractGraph, EdgeManager}

/** simple implementation of WALA graph interface with wasteful memory usage but fast lookups of successor and
  * predecessor nodes */
final class GraphImpl[T] extends AbstractGraph[T]() {

  private val nodeManager = new BasicNodeManager[T]

    val edgeManager = new EdgeManager[T] {
      private val succMap = new java.util.HashMap[T,java.util.Set[T]]()
      private val predMap = new java.util.HashMap[T,java.util.Set[T]]()

      private def getOrElseEmpty(m : java.util.Map[T,java.util.Set[T]], k : T) : java.util.Set[T] = {
        val l = m.get(k)
        if (l != null) l else java.util.Collections.emptySet()
      }

      override def getSuccNodeCount(p1: T): Int = getOrElseEmpty(succMap, p1).size()

      override def getPredNodeCount(p1: T): Int = getOrElseEmpty(predMap, p1).size()

      override def getSuccNodes(p1: T): java.util.Iterator[T] = getOrElseEmpty(succMap, p1).iterator()

      override def getPredNodes(p1: T): java.util.Iterator[T] = getOrElseEmpty(predMap, p1).iterator()

      override def hasEdge(p1: T, p2: T): Boolean = getOrElseEmpty(succMap, p1).contains(p2)

      override def addEdge(p1: T, p2: T): Unit = {
        nodeManager.addNode(p2)
        val p1Succs = succMap.get(p1)
        if (p1Succs == null) {
          nodeManager.addNode(p1)
          val l = new java.util.HashSet[T]()
          l.add(p2)
          succMap.put(p1, l)
        } else p1Succs.add(p2)
      }


      override def removeEdge(p1: T, p2: T): Unit = sys.error("Unimp: removeEdge")
      override def removeIncomingEdges(p1: T): Unit = sys.error("Unimp: removeIncomingEdges")
      override def removeOutgoingEdges(p1: T): Unit = sys.error("Unimp: removeOutgoingEdges")
      override def removeAllIncidentEdges(p1: T): Unit = sys.error("Unimp: removeAllIncidentEdges")
    }

    override def getNodeManager = nodeManager
    override def getEdgeManager = edgeManager

}
