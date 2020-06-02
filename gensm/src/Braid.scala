package gensm

import SGraph._

case class Braid(braidList: List[Int]) {thisBraid =>

    val braid = braidList

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
        val n = (braid.map(x => x.abs)).max + 1
        braidToPermHelper(braid, (1.to(n)).toList)
    }


    lazy val cycleDecomp: List[List[Int]] = {
        val perm = thisBraid.braidToPerm
        val n = perm.length
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
    
    def edgeSignListMaker(braid: List[Int], accum: List[List[Int]]): List[List[Int]] = {
        if(braid.isEmpty) accum
        else edgeSignListMaker(braid.tail, accum.slice(0, braid.head.abs-1) ++ 
        ((accum(braid.head.abs-1) :+ (braid.head.signum)) +: accum.slice(braid.head.abs, accum.length+1))) 
    }

    def singleColoredLinkToGraph(p: Braid): SGraph = {

        val vertexList = makeVertices(0, p.braidToPerm.length)

        val edgeSignList:List[List[Int]] = edgeSignListMaker(p.braid, 
        ((1.to(p.braidToPerm.length-1)).toList).map(x => List[Int]()))

        val edgeListSegregated = ((0.to(p.braidToPerm.length-2)).toList).map(y => makeEdges(vertexList(y), 
        vertexList(y+1), 0, edgeSignList(y).length, 
        edgeSignList(y)))

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
        
        SGraph(vertexList, edgeList, edgeOrder, vertexOrder _)

    }


} 

/*
def edgeSelfIntersection(edge: SEdge, sign: Int, graph: SGraph): Int = {
            if(sign == 1) {
                if(edge == graph.vve(edge.initial,edge.terminal))
            }
            
        }

{
import gensm._
val p = List(1,1,1)
val linkBraid = Braid(p)
val sgraph = Braid.singleColoredLinkToGraph(linkBraid)
val Matrix = sgraph.singleColoredLinkSeifertMatrix
}

*/
