package gensm

import SGraph._

case class Braid(braidList: List[Int], strands: Int) {thisBraid =>

/**    def makeConnected(n: Int, lst: List[Int]): List[Int] = {
        val extra = (1.to(n).toList).map(x => 
        (x.to(n).toList ++ (x.to(n).toList.reverse.map(y => -y)))).flatMap(x=>x)
        lst ++ extra
    }
    */  

    val braid = braidList /** makeConnected(braidList.map(x => x.abs).max, braidList) */

    lazy val braidToPerm: List[Int] = {
        def braidToPermHelper(braid: List[Int], accum: List[Int]): List[Int] = {
            if(braid.isEmpty) accum
            else {
                val newAccum = accum.slice(0, braid.head.abs-1) ++ 
                List(accum(braid.head.abs), accum(braid.head.abs - 1)) ++ 
                accum.slice(braid.head.abs+1, accum.length)
                braidToPermHelper(braid.tail, newAccum)
            }
        }
        val n = strands
        braidToPermHelper(braid, (1.to(n)).toList)
    }


    lazy val cycleDecomp: List[List[Int]] = {
        val perm = thisBraid.braidToPerm
        val n = strands
        def cycleDecompHelper(tally: List[Int], accum: List[List[Int]]): List[List[Int]]= {
            if(tally.isEmpty) accum
            else {
                def cycleMaker(perm: List[Int], cyc: List[Int], current: Int): List[Int] = {
                    if(cyc contains perm(current -1)) cyc
                    else cycleMaker(perm, cyc :+ perm(current -1), perm(current -1))
                }
                val newCyc = cycleMaker(perm, List[Int](tally.head), tally.head)
                cycleDecompHelper(tally.filterNot(newCyc contains _), accum :+ newCyc)
            }
        }
        cycleDecompHelper((1.to(n)).toList, List[List[Int]]())
    }

    lazy val ctComponents: Int = (thisBraid.cycleDecomp).length


}

object Braid {
    import SGraph._ 

    def genVertices(p: Braid, colList: List[Int]): List[SVertex] = {
        val cyc = p.cycleDecomp
        val cycByColor = (0.to(colList.max).toList).map(x => 
            cyc.filter(y => (colList(cyc.indexOf(y)) == x)).flatMap(y => y).sorted)
        cycByColor.map(x => x.map(y => SVertex(cycByColor.indexOf(x), 
            x.indexOf(y)+
            cycByColor.filter(
                z => (cycByColor.indexOf(z) < cycByColor.indexOf(x)
                )).map(l => l.length).fold(0)((a,b) => a+b)))).flatMap(t => t)
    }

    def transpose[A](n: Int, lst: List[A]): List[A] = {
        ((lst.slice(0, n) :+ lst(n+1)) :+ lst(n)) ++ lst.slice(n+2, lst.length+2)
    }
    
    def makeGraph(p: Braid, colList: List[Int], colSigns: List[Int]): SGraph = {
        val vert = genVertices(p, colList)
        val initGraph = SGraph(vert, 
        List[SEdge](), ((x,y) => List[SEdge]()), (_ => List[SEdge]()), colSigns)
        val cyc = p.cycleDecomp

        val flattenedCycByColor = (0.to(colList.max).toList).map(x => 
            cyc.filter(y => (colList(cyc.indexOf(y)) == x)).flatMap(y => y).sorted).flatMap(x => x)
        val vertPerm = (1.to(p.braidToPerm.length).toList).map(x => vert(flattenedCycByColor.indexOf(x)))

        def graphMaker(braid: List[Int], vertPerm: List[SVertex], graph: SGraph): SGraph = {
            if(braid.isEmpty) {
                if (colList.max == 0) graph
                else{
                    val colors = 0.to(colList.max).toList
                    val colInitVerts = colors.map(x => vert.filter(y => (y.col == x))(0))
                    val disconnectedColors = (colors).map(x => 
                        colors.filter(y => (y > x)).map(y => (x,y))).flatMap(x => x).filter(x => 
                        graph.edges.filter(e => ((e.initial.col, e.terminal.col) == x)).isEmpty)
                    def makeEdges(i: Int, j: Int, graph: SGraph): SGraph = {
                        graph.addEdge(
                            colInitVerts(i), colInitVerts(j), 3, i).addEdge(
                                colInitVerts(i), colInitVerts(j), -3, i)
                    }
                    
                    def makeComplete(graph: SGraph,  disconnectedColors: List[(Int, Int)]): SGraph = {
                        if (disconnectedColors.isEmpty) graph 
                        else {
                            val currentPair = disconnectedColors.head
                            makeComplete(makeEdges(currentPair._1, currentPair._2, graph), disconnectedColors.tail)
                        }
                    }
                    makeComplete(graph, disconnectedColors)
                }
            }
            else{
            val i = braid.head.abs
            if(vertPerm(i).col == vertPerm(i-1).col) {
                graphMaker(braid.tail, vertPerm, 
                graph.addEdge(vertPerm(i-1), vertPerm(i), braid.head.signum, vertPerm(i).col))
            }
            else {
                val j = if (braid.head.signum == 1) i else i-1
                val k = 2*i - 1 -j
                val kCol = vertPerm(k).col
                if (vertPerm(j).col < vertPerm(k).col) graphMaker(braid.tail, transpose(i-1, vertPerm), graph)
                else {
                    val clasps = vertPerm.filter(x => 
                    ((vertPerm.indexOf(x) < i-1) && (x.col > vertPerm(k).col) && (x.num < vertPerm(j).num)))
                    if(clasps.isEmpty) graphMaker(braid.tail, transpose(i-1, vertPerm), 
                        graph.addEdge(vertPerm(k), vertPerm(j), braid.head.signum*4, kCol))
                    else if(clasps.length == 1) {
                        val newGraph = graph.addEdge(
                            vertPerm(k), clasps(0), 3, kCol
                            ).addEdge(
                                clasps(0), vertPerm(j), braid.head.signum*5, kCol
                            ).addEdge(
                                vertPerm(k), clasps(0), -3, kCol
                            )
                            graphMaker(braid.tail, transpose(i-1, vertPerm), newGraph)
                    }
                    else {
                        val newGraph = graph.addEdge(
                            vertPerm(k), clasps(0), 3, kCol
                            ).addEdges(
                                clasps, 2, kCol
                            ).addEdge(
                                clasps(0), vertPerm(j), braid.head.signum*5, kCol
                            ).addEdge(
                                vertPerm(k), clasps(0), -3, kCol
                            ).addEdges(
                                clasps, -2, kCol
                            )
                        graphMaker(braid.tail, transpose(i-1, vertPerm), newGraph)
                    }
                    }
                }
            }
        }
        val preGraph = graphMaker(p.braid, vertPerm, initGraph)

        def makeConnected(graph: SGraph, notConnected: List[(SVertex, SVertex)]): SGraph = {
            if(notConnected.isEmpty) graph
            else {
            val init = notConnected.head._1
            val term = notConnected.head._2
            makeConnected(graph.addInitEdge(init, term, -1, init.col).addInitEdge(
                init, term, 1, init.col), notConnected.tail)
            }
        }

        val colVert = (0.to(colList.max).toList).map(x => 
            vert.filter(y => y.col == x).init.zip(vert.filter(y => y.col == x).tail))
        val notConnected = colVert.map(x => x.filter(y => 
            (preGraph.vve(y._1, y._2)).filter(e => e.typ.abs == 1).isEmpty)).flatMap(x => x)
        makeConnected(preGraph, notConnected)
    }
     



    
/** 
    def edgeTypListMaker(braid: List[Int], accum: List[List[Int]]): List[List[Int]] = {
        if(braid.isEmpty) accum
        else edgeTypListMaker(braid.tail, accum.slice(0, braid.head.abs-1) ++ 
        ((accum(braid.head.abs-1) :+ (braid.head.signum)) +: accum.slice(braid.head.abs, accum.length+1))) 
    }

    def singleColoredLinkToGraph(p: Braid): SGraph = {

        val vertexList = makeVertices(0, 0, p.braidToPerm.length)

        val edgeTypList:List[List[Int]] = edgeTypListMaker(p.braid, 
        ((1.to(p.braidToPerm.length-1)).toList).map(x => List[Int]()))

        val edgeListSegregated = ((0.to(p.braidToPerm.length-2)).toList).map(y => makePlusEdges(vertexList(y), 
        vertexList(y+1), 0, edgeTypList(y).length, 
        edgeTypList(y)))

        val edgeList = edgeListSegregated.flatMap(x => x)

        def edgeOrder(initial: SVertex, terminal: SVertex): List[SEdge]  = {
            if(vertexList.indexOf(initial)+1 == vertexList.indexOf(terminal)) 
                edgeListSegregated(vertexList.indexOf(initial))
            else if (vertexList.indexOf(initial) == vertexList.indexOf(terminal)+1)
                edgeListSegregated(vertexList.indexOf(terminal))
            else List[SEdge]()
        }

        def vertexOrder(vert: SVertex): List[SEdge] = {
            def vertexOrderHelper(tally: List[Int], accum: List[SEdge], 
            edgeListSeg: List[List[SEdge]]): List[SEdge] = {
                if(tally.isEmpty) accum
                else {
                    if((vertexList(tally.head.abs) == vert) || (vertexList(tally.head.abs-1) == vert))
                        vertexOrderHelper(tally.tail,
                        accum :+ edgeListSeg(tally.head.abs-1).head,
                        edgeListSeg.map(x => (if(x == edgeListSeg(tally.head.abs-1)) x.tail else x)))
                    else vertexOrderHelper(tally.tail, accum, edgeListSeg)
                }
            }
            vertexOrderHelper(p.braid, List[SEdge](), edgeListSegregated)
        }
        
        SGraph(vertexList, edgeList, edgeOrder, vertexOrder _, List(1))

    }*/


}

/**
def edgeSelfIntersection(edge: SEdge, typ: Int, graph: SGraph): Int = {
            if(typ == 1) {
                if(edge == graph.vve(edge.initial,edge.terminal))
            }
            
        }

{
import gensm._
val p = List(1,1,1)
val linkBraid = Braid(p)
val sgraph = Braid.singleColoredLinkToGraph(linkBraid)`
val Matrix = sgraph.singleColoredLinkSeifertMatrix
}

{import gensm._
import Braid._
val p = Braid(List(1,2,3,4,-1,5,1,-5,-3,-4,-1,2,-1))
makeGraph(p, List(0,1,0), List(1,-1))
}

{import gensm._
  import Braid._
  val p = Braid(List(1,1,1,2,2,3,-3))
  val sgraph = makeGraph(p, List(0,1,2), List(1,-1,-1))
  val hombasis = sgraph.homologyBasis
 val (e1, e2, e3) = (hombasis(3).l.head.head, hombasis(4).l.head.head, hombasis(4).l(1).head)  
 sgraph.intersectLoops(hombasis(3), hombasis(4), List(1,1,1))    
  }            

*/