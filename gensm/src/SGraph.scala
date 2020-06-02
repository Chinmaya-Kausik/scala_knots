package gensm

case class SVertex(n: Int) {
    val num = n
}

case class SEdge(initVertex: SVertex, termVertex: SVertex, n: Int, edgeSign: Int) {
    val initial = initVertex
    val terminal = termVertex
    val num = n
    val sign = edgeSign
}

case class SGraph(vertices: List[SVertex], edgeList: List[SEdge], edgeOrder: 
((SVertex, SVertex) => List[SEdge]), vertexOrder: (SVertex => List[SEdge])) {thisGraph =>
    val vert = vertices
    val edges = edgeList
    val vve = edgeOrder
    val ve = vertexOrder

    def maxEdge(initial: SVertex, terminal: SVertex): SEdge = {
        require((vert contains initial), s"The vertex $initial is not in the graph $thisGraph")
        require((vert contains terminal), s"The vertex $terminal is not in the graph $thisGraph")
        ((vve(initial, terminal).sortBy(_.num)).reverse)(0)
    }
 
    lazy val singleColoredLinkHomotopyBasis: List[(List[SEdge], List[SEdge])] = {
        val edgeListSegregated = vert.init.map(x => vve(x, vert(vert.indexOf(x)+1)).init)
        val maxEdges = vert.init.map(x => maxEdge(x, vert(vert.indexOf(x)+1)))
        edgeListSegregated.map(x => x.map(y => 
        (List(y), List(maxEdges(edgeListSegregated.indexOf(x)))))).flatMap(x => x)
    }
 
    def edgeIndexInit(edge: SEdge) = {
        require(edges.contains(edge), s"The edge $edge is not in the graph $thisGraph")
        ve(edge.initial).indexOf(edge)
    }

    def edgeIndexTerm(edge: SEdge) = {
        require(edges.contains(edge), s"The edge $edge is not in the graph $thisGraph") 
        ve(edge.terminal).indexOf(edge)
    }

    def edgeSelfIntersection(edge: SEdge, lr: Int): Int = {
        if(lr == 0) {
            if(edge.sign == 1) -1
            else 0 
        }
        else {
            if(edge.sign == 1) 0
            else 1  
        }
    }

    def topBetween(edge1: SEdge, edge2: SEdge, edge: SEdge): Boolean = {
        if(edge1.initial == edge.terminal) ((edgeIndexInit(edge1) <= edgeIndexTerm(edge)) && 
        (edgeIndexInit(edge2) >= edgeIndexTerm(edge)))
        else false
    }

    def bottomBetween(edge1: SEdge, edge2: SEdge, edge: SEdge): Boolean = {
        if(edge1.terminal == edge.terminal) ((edgeIndexTerm(edge1) <= edgeIndexTerm(edge)) && 
        (edgeIndexTerm(edge2) >= edgeIndexTerm(edge)))
        else false
    }

    def topIntersection(edge1: SEdge, edge2: SEdge, edge: SEdge, dir: Int): Int = {
        if(edge1.initial == edge.terminal) {
            if (topBetween(edge1, edge2, edge)) -dir
            else 0
        }
        else 0
    }

    def bottomIntersection(edge1: SEdge, edge2: SEdge, edge: SEdge, dir: Int): Int = {
        if(edge1.terminal == edge.terminal) {
            if (bottomBetween(edge1, edge2, edge)) dir
            else 0
        }
        else 0
    }

    def checkLoop(loop: (List[SEdge], List[SEdge])): Boolean = {
        val left = loop._1.init.map(x => 
        (x.initial == (loop._1(loop._1.indexOf(x)+1)).terminal)).fold(true)((a,b) => a && b)
        val right = loop._2.init.map(x => 
        (x.terminal == (loop._2(loop._2.indexOf(x)+1)).initial)).fold(true)((a,b) => a && b)
        val top = (loop._1.last.initial == loop._2.head.initial)
        val bottom = (loop._1.head.terminal == loop._2.head.terminal)
        left && right && top && bottom

    }

    def positiveLiftLinkingNumber(loop1: (List[SEdge], List[SEdge]), loop2: (List[SEdge], List[SEdge])): Int = {
        require(checkLoop(loop1), s"$loop1 is not a loop")
        require(checkLoop(loop2), s"$loop2 is not a loop")
        val leftList = (loop1._1.toSet intersect loop2._1.toSet).toList
        val rightList = (loop1._2.toSet intersect loop2._2.toSet).toList
        val left = leftList.map(x => edgeSelfIntersection(x, 0)).fold(0)((a,b) => a+b)
        val right = rightList.map(x => edgeSelfIntersection(x, 1)).fold(0)((a,b) => a+b)
        val topList = loop2._1.map(x => topIntersection(loop1._1.last, loop1._2.head, x, 1)) ++ 
            loop2._2.map(x => topIntersection(loop1._1.last, loop1._2.head, x, -1))
        val bottomList = loop2._1.map(x => bottomIntersection(loop1._1.head, loop1._2.last, x, 1)) ++
            loop2._2.map(x => bottomIntersection(loop1._1.head, loop1._2.last, x, -1))
        val top = topList.fold(0)((a,b) => a+b)
        val bottom = bottomList.fold(0)((a,b) => a+b)
        left + right + top + bottom
    }

    lazy val singleColoredLinkSeifertMatrix: List[List[Int]] = {
        val loopPairs = singleColoredLinkHomotopyBasis.map(x => singleColoredLinkHomotopyBasis.map(y => (x,y)))
        loopPairs.map(x => x.map(y => positiveLiftLinkingNumber(y._2, y._1)))
    }
    
}

object SGraph {
    def makeVertices(n: Int, m: Int): List[SVertex] = {
            if(n>=m) List[SVertex]()
            else SVertex(n) +: makeVertices(n+1, m)
        }
    
    def makeEdges(initial: SVertex, terminal: SVertex, n: Int, m: Int, signList: List[Int]): List[SEdge] = {
        require((signList.length == m-n), s"The list $signList does not have the required length")
        if(n>=m) List[SEdge]()
        else SEdge(initial, terminal, n,signList.head) +: makeEdges(initial, terminal, n+1, m, signList.tail)
    }
} 




