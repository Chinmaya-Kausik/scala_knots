package gensm

case class SVertex(color: Int, n: Int) {
    val num = n
    val col = color
}

case class SEdge(initVertex: SVertex, termVertex: SVertex, n: Int, edgeTyp: Int, 
    backgroundCol: Int) {
    val initial = initVertex
    val terminal = termVertex
    val num = n
    val typ = edgeTyp
    val col = backgroundCol
}

case class SGraph(vertices: List[SVertex], edgeList: List[SEdge], edgeOrder: 
((SVertex, SVertex) => List[SEdge]), vertexOrder: (SVertex => List[SEdge]), colourSigns: List[Int]) {thisGraph =>
    val vert = vertices
    val edges = edgeList
    val vve = edgeOrder
    val ve = vertexOrder
    val colSigns = colourSigns

    def addEdge(init: SVertex, term: SVertex, typ: Int, col: Int): SGraph = {
        val newEdge = SEdge(init, term, vve(init, term).length, typ, col)
        SGraph(vert, edges :+ newEdge, ((initial, terminal) => if 
        ((initial == init) && (terminal == term)) 
        (vve(initial, terminal) :+ newEdge) else vve(initial, terminal)), 
        (v => if ((v == init) || (v == term))
        (ve(v) :+ newEdge)
        else ve(v)),
        colSigns)
    }
    
    def addInitEdge(init: SVertex, term: SVertex, typ: Int, col: Int): SGraph = {
        val newEdge = SEdge(init, term, vve(init, term).length, typ, col)
        SGraph(vert, newEdge +: edges, ((initial, terminal) => if 
        ((initial == init) && (terminal == term)) 
        (newEdge +: vve(initial, terminal)) else vve(initial, terminal)), 
        (v => if ((v == init) || (v == term))
        (newEdge +: ve(v))
        else ve(v)),
        colSigns)

    }

    def addEdges(vertices: List[SVertex], typ: Int, col: Int): SGraph = {
        if(vertices.length < 2) thisGraph
        else thisGraph.addEdge(vertices(0), vertices(1), typ, col).addEdges(vertices.tail, typ, col)
    }

    case class Loop(loopList: List[List[SEdge]]) {
        require(checkLoop(loopList), s"$loopList is not a loop")
        val l = loopList

    }

    def checkLoop(loop: List[List[SEdge]]): Boolean = {
        def checkPath(path: List[SEdge]): Boolean = {
            path.init.zip(path.tail).map(x => (x._1.terminal == x._2.initial)).fold(true)((a,b) => a && b)
        }
        val pathCheck = loop.map(x => checkPath(x)).fold(true)((a,b) => a && b)
        val turns = loop.zip(loop.tail :+ loop.head)
        val turnCheck = (0.to(turns.length -1).toList).map(x => if(x %2 == 0) 
            (turns(x)._1.last.terminal == turns(x)._2.last.terminal) else 
            (turns(x)._1.head.initial == turns(x)._2.head.initial)).fold(true)((a,b) => a && b)
        pathCheck && turnCheck
    }


    def maxEdge(initial: SVertex, terminal: SVertex): SEdge = {
        require((vert contains initial), s"The vertex $initial is not in the graph $thisGraph")
        require((vert contains terminal), s"The vertex $terminal is not in the graph $thisGraph")
        (vve(initial, terminal)).last
    }
    
    def findLeft(init: SVertex, term: SVertex): SEdge = vve(init, term).filter(
        x => (List(1,-1) contains (x.typ))).head

    def findRight(init: SVertex, term: SVertex): SEdge = vve(init, term).filter(
        x => (List(1,-1) contains (x.typ))).last


    lazy val maxConnectors: List[List[SEdge]] = {
        val colors = (0.to(colSigns.length -1)).toList
        colors.map(x => colors.filter(y => (y > x)).map(y => 
            (x,y))).map(x => x.map(y => edges.filter(e => ((e.initial.col, e.terminal.col) == y)).last))
    }

    def makeInitialPath(v1: SVertex, v2: SVertex): List[SEdge] = {
        val vert1 = if (v1.num <= v2.num) v1 else v2
        val vert2 = if (v1.num <= v2.num) v2 else v1
        val verts = vert.filter(x => ((x.num >= vert1.num) && (x.num <= vert2.num)))
        verts.init.zip(verts.tail).map(x => findLeft(x._1, x._2))

    }

    def makeTerminalPath(v1: SVertex, v2: SVertex): List[SEdge] = {
        val vert1 = if (v1.num <= v2.num) v1 else v2
        val vert2 = if (v1.num <= v2.num) v2 else v1
        val verts = vert.filter(x => ((x.num >= vert1.num) && (x.num <= vert2.num)))
        verts.init.zip(verts.tail).map(x => findRight(x._1, x._2))

    }

    def collapsePaths(paths: List[List[SEdge]]): List[List[SEdge]] = {
        if (paths.length <= 1) paths
        else {
            if (paths.head.isEmpty) collapsePaths(paths.tail)
            else if (paths(1).isEmpty) collapsePaths(paths.head +: paths.tail.tail)
            else {
                if (paths.head.last.terminal == paths(1).head.initial) collapsePaths(
                    (paths.head ++ paths(1)) +: paths.tail.tail)
                else if (paths.head.head.initial == paths(1).last.terminal) collapsePaths(
                    (paths(1) ++ paths.head) +: paths.tail.tail)
                else paths.head +: collapsePaths(paths.tail)
            }
        }
    }
    

    def makeCompleteGraphHomologyLoop(e1: SEdge, e2: SEdge, e3: SEdge): Loop = {
        if (e1.initial.num <= e3.initial.num) Loop(
            collapsePaths(List(List(e1), makeInitialPath(e1.terminal, e2.initial), List(e2), 
            makeTerminalPath(e2.terminal, e3.terminal), makeInitialPath(e3.initial, e1.initial) :+ e3)))
        else Loop(
            collapsePaths(List(makeInitialPath(e3.initial, e1.initial) :+ e1, 
            makeInitialPath(e1.terminal, e2.initial), List(e2), 
            makeTerminalPath(e2.terminal, e3.terminal), List(e3))))
        
    }

    def maxConnectorCorrector(e: SEdge): SEdge = {
        if (List(-2,-3) contains e.typ) vve(e.initial, e.terminal).filter(x => x.typ == e.typ.abs).last
        else e
    }

    lazy val completeGraphHomologyBasis: List[Loop] = {
        val colors = (0.to(colSigns.length -1)).toList
        val ijMatches = colors.tail.map(x => colors.tail.filter(y => y > x).map(y => (x,y))).flatMap(x => x)
        ijMatches.map(x => 
            makeCompleteGraphHomologyLoop(
            maxConnectorCorrector(maxConnectors(0)(x._1-1)), 
            maxConnectorCorrector(maxConnectors(x._1)(x._2-x._1-1)), 
            maxConnectorCorrector(maxConnectors(0)(x._2-1))))
    }

    def nextEdgeTerm(e: SEdge): SEdge = {
        val vertEdges = ve(e.terminal)
        vertEdges(vertEdges.indexOf(e)+1)
    }

    def previousEdgeInit(e: SEdge): SEdge = {
        val vertEdges = ve(e.initial)
        vertEdges(vertEdges.indexOf(e)-1)
    }

    def continueAbove(paths: List[List[SEdge]]): List[List[SEdge]] = {
        val currentEdge = paths.last.last
        if (currentEdge == ve(currentEdge.terminal).last) paths
        else {
            val nextEdge = nextEdgeTerm(currentEdge) 
            if ((nextEdge.initial == currentEdge.terminal) && 
                (nextEdge.terminal.col == currentEdge.terminal.col)) {
                if (List(2,-2) contains nextEdge.typ) continueAbove(paths :+ (paths.last :+ nextEdge))
                else if(List(5,-5) contains nextEdge.typ) (paths :+ (paths.last :+ nextEdge))
                else paths
        }
        else paths

        }
        
    }

    def continueBelow(paths: List[List[SEdge]]): List[List[SEdge]] = {
        val currentEdge = paths.last.head
        if (currentEdge == ve(currentEdge.initial).head) paths
        else {
            val previousEdge = previousEdgeInit(currentEdge)
            if ((previousEdge.terminal == currentEdge.initial) && 
                (previousEdge.initial.col == currentEdge.initial.col) && 
                (List(2,-2,5,-5) contains currentEdge.typ)) {
                if (List(2,-2) contains previousEdge.typ) continueBelow(paths :+ (previousEdge +: paths.last))
                else if (List(3,-3) contains previousEdge.typ) (paths :+ (previousEdge +: paths.last))
                else paths
            } 
            else paths
        }
    }
    
    def makeLoop(paths: List[SEdge]): Loop = {
        val vert1 = paths.head.initial
        val vert2 = paths.last.terminal
        val colEdge = maxConnectors(vert1.col)(vert2.col - vert1.col -1)
        val edgesBelow = makeInitialPath(vert1, colEdge.initial)
        val rightEdges = makeTerminalPath(vert2, colEdge.terminal)
        if (vert1.num <= colEdge.initial.num) Loop(collapsePaths(List(paths, rightEdges, edgesBelow :+ colEdge)))
        else Loop(collapsePaths(List(edgesBelow ++ paths, rightEdges, List(colEdge))))
    }

    def localGraphBasis(init: Int, term: Int): List[Loop] = {
        val edgesBetween = edges.filter(x => (((x.initial.col, x.terminal.col) == (init, term)) && 
        (x != maxConnectors(init)(term - init -1))))
        val paths = edgesBetween.map(x => continueAbove(List(List(x))) ++ continueBelow(List(List(x))).tail).flatMap(x => x)
        paths.map(x => makeLoop(x))
    }

    lazy val allLocalGraphBases: List[Loop] = {
        val colors = (0.to(colSigns.length-1)).toList
        colors.map(x => colors.filter(y => (x < y)).map(y => (x,y))).flatMap(
            x => x).map(x => localGraphBasis(x._1, x._2)).flatMap(x => x)
    }

    def singleColorBasis(color: Int): List[Loop] = {
        val colorVerts = vert.filter(x => x.col == color)
        val leftEdges = colorVerts.init.zip(colorVerts.tail).map(x => 
            vve(x._1, x._2).filter(e => List(1,-1) contains e.typ).init).flatMap(x => x)
        leftEdges.map(e => Loop(List(List(e), List(findRight(e.initial, e.terminal)))))
    }

    lazy val allSingleColorBases: List[Loop] = {
        val colors = (0.to(colSigns.length-1)).toList
        colors.map(x => singleColorBasis(x)).flatMap(x => x)
    }
 
    lazy val homologyBasis: List[Loop] = completeGraphHomologyBasis ++ allLocalGraphBases ++ allSingleColorBases
    
    
    def leftRight(e: SEdge, colourDir: List[Int]): Int = {
            val sgn = colourDir(e.col)*colSigns(e.col)
            val lrSigns = List(1,1,1,-1,-1)
            val lr = e.typ.signum*lrSigns(e.typ.abs-1)*sgn
            if (lr == 1) 1
            else 0
        }

    def upDown(v: SVertex, colourDir: List[Int]): Int = {
        if (colourDir(v.col)*colSigns(v.col) == 1) 0
        else 1
    }

    def termMain(e: SEdge, colourDir: List[Int], ru1: Int, ru2: Int, relSign: Int): Int = {
        if(ru1 == 0) {
            if(leftRight(e, colourDir) == 1) List(
                List(-1,-1), List(0,-1))(upDown(e.terminal, colourDir))(ru2)*relSign
            else 0
        }
        else 0
    }

    def initMain(e: SEdge, colourDir: List[Int], ru1: Int, ru2: Int, relSign: Int): Int = {
        if(ru1 == 0) {
            if(leftRight(e, colourDir) == 1) List(
                List(0,1), List(1,1))(upDown(e.initial, colourDir))(ru2)*relSign
            else 0
        }
        else 0
    }

    def verticalIntersection(e: SEdge, colourDir: List[Int], relSign: Int): Int = {
        if (e.typ == 5) {
            val sgn = colourDir(e.col)*colSigns(e.col)
            if(sgn == -1) relSign
            else 0
        }
        else 0
    }

    def termLeftIntersection(e: SEdge, colourDir: List[Int], ru1: Int, ru2: Int, l1: Int, l2: Int, relSign: Int): Int = {
        if (ru1 == 0) {
            val sgn = upDown(e.terminal, colourDir)
            if (sgn == 1) {
                if ((l1,l2,ru2) == (1,0,1)) relSign
                else 0
            }
            else {
                if (l1 == 1) List(List(1,1),List(1,0))(l2)(ru2)*relSign
                else 0
            }
        }
        else 0
    }

    def initLeftIntersection(e: SEdge, colourDir: List[Int], ru1: Int, ru2: Int, l1: Int, l2: Int, relSign: Int): Int = {
        if (ru1 == 0) {
            val sgn = upDown(e.initial, colourDir)
            if (sgn == 0) {
                if ((l1,l2,ru2) == (1,0,1)) -relSign
                else 0
            }
            else {
                if (l1 == 1) List(List(-1,-1),List(-1,0))(l2)(ru2)*relSign
                else 0
            }
        }
        else 0
    }

    def edgeIndexInit(edge: SEdge): Int = {
        require(edges.contains(edge), s"The edge $edge is not in the graph $thisGraph")
        ve(edge.initial).indexOf(edge)
    }

    def edgeIndexTerm(edge: SEdge): Int = {
        require(edges.contains(edge), s"The edge $edge is not in the graph $thisGraph") 
        ve(edge.terminal).indexOf(edge)
    }

    def termRightUp(e: SEdge, loop: Loop, pathIndex: Int): Int = {
        val lst = loop.l
        if ((lst.map(x => x.last)) contains e) 0
        else if (ve(e.terminal).last == e) {
            val path = lst(pathIndex)
            val pathNextEdge = path(path.indexOf(e)+1)
            val anomalousCaseTypes = (pathNextEdge.typ.abs == 5) && (List(-2,-3) contains e.typ)
            if(anomalousCaseTypes) {
                val pathAdjacentEdgeIsTopmost = (pathNextEdge == edges(edges.indexOf(
                    continueBelow(List(List(e))).last.head) -1))
                if (pathAdjacentEdgeIsTopmost) 1
                else 0
            }
            else 0
        }
                
        else {
            val path = lst(pathIndex)
            val nextEdge = nextEdgeTerm(e)
            val nextEdgeIsPathAdjacent = (nextEdge == path(path.indexOf(e)+1))
            val edgesConnect = (List(2,-2,3,-3) contains e.typ) && (List(2,-2,5,-5) contains nextEdge.typ)
            if (nextEdgeIsPathAdjacent && edgesConnect) 1
            else 0  
        }
    }

    def initRightUp(e: SEdge, loop: Loop, pathIndex: Int): Int = {
        val lst = loop.l
        if ((lst.map(x => x.head) :+ ve(e.initial).head) contains e) 0     
        else {
            val path = lst(pathIndex)
            val previousEdge = previousEdgeInit(e)
            val previousEdgeIsPathAdjacent = (previousEdge == path(path.indexOf(e)-1))
            val edgesConnect = (List(2,-2,5,-5) contains e.typ)
            if (previousEdgeIsPathAdjacent && edgesConnect) 1
            else 0
        }
    }

    def termLeft(e: SEdge, loop: Loop, pathIndex: Int): Int = {
        val lst = loop.l
        if (termRightUp(e, loop, pathIndex) == 0) {
            val path = lst(pathIndex)
            if ((pathIndex %2 == 0) && (e == path.last)) {
                val nextEdge = lst((pathIndex +1) %lst.length).last
                if (edgeIndexTerm(nextEdge) < edgeIndexTerm(e)) 1
                else 0
            }
            else if ((pathIndex %2 == 1) && (e == path.last)) {
                val previousEdge = lst((pathIndex - 1 + lst.length) %lst.length).last
                if (edgeIndexTerm(previousEdge) < edgeIndexTerm(e)) 1
                else 0
            }
            else {
            val nextEdge = path(path.indexOf(e)+1)
            if (edgeIndexInit(nextEdge) < edgeIndexTerm(e)) 1
            else 0
            }
        }
        else 0
    }

    def initLeft(e: SEdge, loop: Loop, pathIndex: Int): Int = {
        val lst = loop.l
        if (initRightUp(e, loop, pathIndex) == 0) {
            val path = lst(pathIndex)
            if ((pathIndex %2 == 0) && (e == path.head)) {
                val previousEdge = lst((pathIndex -1 +lst.length) %lst.length).head
                if (edgeIndexInit(previousEdge) < edgeIndexInit(e)) 1
                else 0
            }
            else if ((pathIndex %2 == 1) && (e == path.head)) {
                val nextEdge = lst((pathIndex +1) %lst.length).head 
                if (edgeIndexInit(nextEdge) < edgeIndexInit(e)) 1
                else 0
            }
            else {
            val previousEdge = path(path.indexOf(e)-1)
            if (edgeIndexTerm(previousEdge) < edgeIndexInit(e)) 1
            else 0
            }
        }
        else 0
    }

    def edgeSelfIntersectionsCalc(e: SEdge, pathIndex1: Int, pathIndex2: Int, loop1: Loop, 
        loop2: Loop, colourDir: List[Int], relSign: Int): List[Int] = {

        val terml1 = termLeft(e, loop1, pathIndex1)
        val terml2 = termLeft(e, loop2, pathIndex2)
        val initl1 = initLeft(e, loop1, pathIndex1)
        val initl2 = initLeft(e, loop2, pathIndex2)
        if (e.typ.abs == 1) {
            val sgn = colSigns(e.col)*colourDir(e.col)
            val main =  {
                if (sgn == 1) {
                    if (e.typ == 1) 0
                    else -1
                }
                else {
                    if (e.typ == 1) 1
                    else 0
                }
            }
            List(main*relSign, termLeftIntersection(e, colourDir, 0, 0, terml1, terml2, relSign),
            initLeftIntersection(e, colourDir, 0, 0, initl1, initl2, relSign))
        } 
        else {
            val termru1 = termRightUp(e, loop1, pathIndex1)
            val termru2 = termRightUp(e, loop2, pathIndex2)
            val initru1 = initRightUp(e, loop1, pathIndex1)
            val initru2 = initRightUp(e, loop2, pathIndex2)
            /** println(List(termMain(e, colourDir, termru1, termru2, relSign),
               initMain(e, colourDir, initru1, initru2, relSign),
               verticalIntersection(e, colourDir, relSign),
               termLeftIntersection(e, colourDir, termru1, termru2, terml1, terml2, relSign),
               initLeftIntersection(e, colourDir, initru1, initru2, initl1, initl2, relSign) ))*/
            List(termMain(e, colourDir, termru1, termru2, relSign),
            initMain(e, colourDir, initru1, initru2, relSign),
            verticalIntersection(e, colourDir, relSign),
            termLeftIntersection(e, colourDir, termru1, termru2, terml1, terml2, relSign), 
            initLeftIntersection(e, colourDir, initru1, initru2, initl1, initl2, relSign))

        }
    }


    def edgeSelfIntersection(e: SEdge, pathIndex1: Int, pathIndex2: Int, loop1: Loop, 
        loop2: Loop, colourDir: List[Int], relSign: Int): Int = {

        val terml1 = termLeft(e, loop1, pathIndex1)
        val terml2 = termLeft(e, loop2, pathIndex2)
        val initl1 = initLeft(e, loop1, pathIndex1)
        val initl2 = initLeft(e, loop2, pathIndex2)
        if (e.typ.abs == 1) {
            val sgn = colSigns(e.col)*colourDir(e.col)
            val main =  {
                if (sgn == 1) {
                    if (e.typ == 1) 0
                    else -1
                }
                else {
                    if (e.typ == 1) 1
                    else 0
                }
            }
            main*relSign + termLeftIntersection(e, colourDir, 0, 0, terml1, terml2, relSign) + 
            initLeftIntersection(e, colourDir, 0, 0, initl1, initl2, relSign)
        } 
        else {
            val termru1 = termRightUp(e, loop1, pathIndex1)
            val termru2 = termRightUp(e, loop2, pathIndex2)
            val initru1 = initRightUp(e, loop1, pathIndex1)
            val initru2 = initRightUp(e, loop2, pathIndex2)
            /** println(List(termMain(e, colourDir, termru1, termru2, relSign),
               initMain(e, colourDir, initru1, initru2, relSign),
               verticalIntersection(e, colourDir, relSign),
               termLeftIntersection(e, colourDir, termru1, termru2, terml1, terml2, relSign),
               initLeftIntersection(e, colourDir, initru1, initru2, initl1, initl2, relSign) ))*/
            termMain(e, colourDir, termru1, termru2, relSign) + 
            initMain(e, colourDir, initru1, initru2, relSign) +
            verticalIntersection(e, colourDir, relSign) + 
            termLeftIntersection(e, colourDir, termru1, termru2, terml1, terml2, relSign) + 
            initLeftIntersection(e, colourDir, initru1, initru2, initl1, initl2, relSign)
        }
    }

    case class Junction(e1: SEdge, e2: SEdge, v: SVertex) {thisJunction =>

        val e1Index = edges.indexOf(e1)
        val e2Index = edges.indexOf(e2)

        val rightLeft = if (e1Index < e2Index) 1 else -1

        val a = if (rightLeft == 1) {
            if ((List(2,-2,3,-3) contains e1.typ) && (List(2,-2,5,-5) contains edges(e1Index+1).typ)) e1Index+1
            else e1Index
        }
        else {
            if (List(2,-2,5,-5) contains e1.typ) e1Index-1
            else e1Index
        } 

        val b = if (rightLeft == 1) {
            if (List(2,-2,5,-5) contains e2.typ) e2Index -1
            else e2Index
        }
        else {
            if ((List(2,-2,3,-3) contains e2.typ) && (List(2,-2,5,-5) contains edges(e2Index+1).typ)) e2Index +1
            else e2Index
        }

        val i1 = if (rightLeft == 1) a else b
        val i2 = b+a-i1

        val active = ((a-b).abs > 1) || !((List(2,-2,3,-3) contains e1) 
            || (List(2,-2,3,-3) contains e2))

        def intersectJunction(j: Junction, colourDir: List[Int]): Int = {
            if (!active) 0
            else {
                val vert = j.v
                if (vert == thisJunction.v) {
                    val sgn = colourDir(vert.col)*colSigns(vert.col)
                    if ((j.i1 > i1) && (j.i1 < i2) && (j.i2 >= i2)) {
                        if (sgn == 1) -rightLeft*j.rightLeft
                        else 0
                    }
                    else if ((j.i1 <= i1) && (j.i2 > i1) && (j.i2 < i2)) {
                        if(sgn == 1) rightLeft*j.rightLeft
                        else 0
                    }
                    else 0
                }
                else 0
            }
        }

        def intersectEdge(e: SEdge, colourDir: List[Int], ud: Int): Int = {
            if (!active) 0
            else {
                if ((e.initial.num <= v.num) && (e.terminal.num > v.num) && 
                (edges.indexOf(e) < i2) && (edges.indexOf(e) > i1)) -ud*rightLeft
                else 0
            }
        }
        
    }

    def makeJunctions(loop: Loop): List[Junction] = {
            val lst = loop.l
            val turns = lst.zip(lst.tail :+ lst.head)
            (0.to(turns.length -1).toList).map(x => if (x %2 == 0) 
            Junction(turns(x)._1.last, turns(x)._2.last, turns(x)._1.last.terminal)
            else Junction(turns(x)._1.head, turns(x)._2.head, turns(x)._1.head.initial)) ++ 
            (0.to(lst.length -1)).map(x => if (x %2 == 0) ((0.to(lst(x).length -2).toList).map(y => 
                Junction(lst(x)(y), lst(x)(y+1), lst(x)(y).terminal))) else 
                ((0.to(lst(x).length -2).toList).map(y => 
                Junction(lst(x)(y+1), lst(x)(y), lst(x)(y).terminal)))).flatMap(x => x)
    }

     def edgeIntersectionsCalc(pathIndex1: Int, pathIndex2: Int, 
        loop1: Loop, loop2: Loop, colourDir: List[Int], relSign: Int): Int = {
            val path1 = loop1.l(pathIndex1)
            val path2 = loop2.l(pathIndex2)
            (path1.toSet intersect path2.toSet).toList.map(x => 
            edgeSelfIntersection(x, pathIndex1, pathIndex2, 
            loop1, loop2, colourDir, relSign)).fold(0)((a,b) => a+b)
    }

    def intersectionsCalc(loop1: Loop, loop2: Loop, colourDir: List[Int]): List[Int] = {
        val junctions1 = makeJunctions(loop1)
        val junctions2 = makeJunctions(loop2)

        val lst1 = loop1.l
        val lst2 = loop2.l
        
        val jjIntersections = junctions1.map(x => junctions2.map(y => 
            x.intersectJunction(y, colourDir))).flatMap(x => x).fold(0)((a,b) => a+b)
        
        val je1Intersections = junctions1.map(x => (0.to(lst2.length -1).toList).map(y => 
            if (y %2 == 0) lst2(y).map(z => 
            x.intersectEdge(z, colourDir, 1)) else List[Int]()).flatMap(y => 
            y)).flatMap(x => x).fold(0)((a,b) => a+b)
        val je2Intersections = junctions1.map(x => (0.to(lst2.length -1).toList).map(y => 
            if (y %2 == 1) lst2(y).map(z => 
            x.intersectEdge(z, colourDir, -1)) else List[Int]()).flatMap(y => 
            y)).flatMap(x => x).fold(0)((a,b) => a+b)
        

        def edgeIntersections(pathIndex1: Int, pathIndex2: Int, 
        loop1: Loop, loop2: Loop, relSign: Int): Int = {
            val path1 = loop1.l(pathIndex1)
            val path2 = loop2.l(pathIndex2)
            (path1.toSet intersect path2.toSet).toList.map(x => 
            edgeSelfIntersection(x, pathIndex1, pathIndex2, 
            loop1, loop2, colourDir, relSign)).fold(0)((a,b) => a+b)
        }

        val pathIndexPairs = (0.to(loop1.l.length -1).toList).map(x => 
            (0.to(loop2.l.length -1).toList).map(y => (x,y))).flatMap(x => x)
        val ee1Intersections = pathIndexPairs.map(x => if (x._1 %2 == x._2 %2) 
            edgeIntersections(x._1, x._2, loop1, loop2, 1) else 0).fold(0)((a, b) => a+ b)
        val ee2Intersections = pathIndexPairs.map(x => if (x._1 %2 != x._2 %2) 
            edgeIntersections(x._1, x._2, loop1, loop2, -1) else 0).fold(0)((a, b) => a+ b)
        
        List(jjIntersections, je1Intersections, je2Intersections, ee1Intersections, ee2Intersections)
        
    }


    
    def intersectLoops(loop1: Loop, 
        loop2: Loop, colourDir: List[Int]): Int = {

        require(checkLoop(loop1.l), s"$loop1 is not a loop")
        require(checkLoop(loop2.l), s"$loop2 is not a loop")

        

        val junctions1 = makeJunctions(loop1)
        val junctions2 = makeJunctions(loop2)

        val lst1 = loop1.l
        val lst2 = loop2.l
        
        val jjIntersections = junctions1.map(x => junctions2.map(y => 
            x.intersectJunction(y, colourDir))).flatMap(x => x).fold(0)((a,b) => a+b)
        
        val je1Intersections = junctions1.map(x => (0.to(lst2.length -1).toList).map(y => 
            if (y %2 == 0) lst2(y).map(z => 
            x.intersectEdge(z, colourDir, 1)) else List[Int]()).flatMap(y => 
            y)).flatMap(x => x).fold(0)((a,b) => a+b)
        val je2Intersections = junctions1.map(x => (0.to(lst2.length -1).toList).map(y => 
            if (y %2 == 1) lst2(y).map(z => 
            x.intersectEdge(z, colourDir, -1)) else List[Int]()).flatMap(y => 
            y)).flatMap(x => x).fold(0)((a,b) => a+b)
        

        def edgeIntersections(pathIndex1: Int, pathIndex2: Int, 
        loop1: Loop, loop2: Loop, relSign: Int): Int = {
            val path1 = loop1.l(pathIndex1)
            val path2 = loop2.l(pathIndex2)
            (path1.toSet intersect path2.toSet).toList.map(x => 
            edgeSelfIntersection(x, pathIndex1, pathIndex2, 
            loop1, loop2, colourDir, relSign)).fold(0)((a,b) => a+b)
        }

        val pathIndexPairs = (0.to(loop1.l.length -1).toList).map(x => 
            (0.to(loop2.l.length -1).toList).map(y => (x,y))).flatMap(x => x)
        val ee1Intersections = pathIndexPairs.map(x => if (x._1 %2 == x._2 %2) 
            edgeIntersections(x._1, x._2, loop1, loop2, 1) else 0).fold(0)((a, b) => a+ b)
        val ee2Intersections = pathIndexPairs.map(x => if (x._1 %2 != x._2 %2) 
            edgeIntersections(x._1, x._2, loop1, loop2, -1) else 0).fold(0)((a, b) => a+ b)
        jjIntersections + je1Intersections + je2Intersections + ee1Intersections + ee2Intersections

    }

    def genSeifertMatrix(colourDir: List[Int]): List[List[Int]] = {
        homologyBasis.map(x => homologyBasis.map(y => (x,y))).map(x => x.map(y => 
        intersectLoops(y._1, y._2, colourDir)))
    }


    def makeSymbolicMatrix(colourDir: List[Int]): List[List[String]] = {
        val product = (0.to(colourDir.length -1).toList).map(x => 
            if (colourDir(x) ==1) ("t"++ x.toString) else "").foldRight("1")((a,b) => 
            if (a != "") (a ++ "*" ++ b) else b)
        val sgn = colourDir.fold(1)((a,b) => a*b)
        genSeifertMatrix(colourDir).map(x => x.map(y =>
            if (y != 0) ((sgn*y).toString ++ "*" ++ product) else "0"))    
    }

    def addSymbols(a: String, b: String): String = {
        if (a == "0") b
        else {
            if (b == "0") a
            else if (b(0) == '-') a++b
            else a ++ "+" ++ b 
        }
    }

    def addSymbolicMatrices(m1: List[List[String]], m2: List[List[String]]): List[List[String]] = {
        if (m1.isEmpty) m2
        else{
            if (m1.head.isEmpty) m2
            else {
                (0.to(m1.length -1).toList).map(x => 
                (0.to(m1(0).length -1)).toList.map(y => addSymbols(m1(x)(y), m2(x)(y))))
            }
        }
        
    }

    def exhaustSignTuples(n: Int): List[List[Int]] = {
        if (n == 1) List(List(1), List(-1))
        else exhaustSignTuples(n-1).map(x => (x :+ 1)) ++ exhaustSignTuples(n-1).map(x => (x :+ -1))
    }

    lazy val presentationMatrix: List[List[String]] = {
        val lst = exhaustSignTuples(colSigns.length)
        lst.map(x => makeSymbolicMatrix(x)).reduce((a,b) => addSymbolicMatrices(a,b))
    }


    
/**    def maxEdge(initial: SVertex, terminal: SVertex): SEdge = {
        require((vert contains initial), s"The vertex $initial is not in the graph $thisGraph")
        require((vert contains terminal), s"The vertex $terminal is not in the graph $thisGraph")
        ((vve(initial, terminal).sortBy(_.num)).reverse)(0)
    }
 
    lazy val singleColoredLinkHomologyBasis: List[Loop] = {
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
            if(edge.typ == 1) -1
            else 0 
        }
        else {
            if(edge.typ == 1) 0
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

    def checkLoop(loop: Loop): Boolean = {
        val left = loop._1.init.map(x => 
        (x.initial == (loop._1(loop._1.indexOf(x)+1)).terminal)).fold(true)((a,b) => a && b)
        val right = loop._2.init.map(x => 
        (x.terminal == (loop._2(loop._2.indexOf(x)+1)).initial)).fold(true)((a,b) => a && b)
        val top = (loop._1.last.initial == loop._2.head.initial)
        val bottom = (loop._1.head.terminal == loop._2.head.terminal)
        left && right && top && bottom

    }

    def positiveLiftLinkingNumber(loop1: Loop, loop2: Loop): Int = {
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
        val loopPairs = singleColoredLinkHomologyBasis.map(x => singleColoredLinkHomologyBasis.map(y => (x,y)))
        loopPairs.map(x => x.map(y => positiveLiftLinkingNumber(y._2, y._1)))
    }
    {
    import gensm._
    import Braid._
    val p = Braid(List(-1,2,-1,2,-1,2), 3)
    val sgraph = makeGraph(p, List(0,1,2), List(1,1,1))
    sgraph
    val List(l1, l2) = sgraph.homologyBasis
    sgraph.intersectLoops(l2,l1, List(1,1))
    val List(e1,e2,e3,e4) = sgraph.edges
    
    
    }

    {
    val e = e2
    val loop1 = l1
    val loop2 = l1
    val pathIndex1 = 0
    val pathIndex2 = 0
    val terml1 = termLeft(e, loop1, pathIndex1)
    val terml2 = termLeft(e, loop2, pathIndex2)
    val initl1 = initLeft(e, loop1, pathIndex1)
    val initl2 = initLeft(e, loop2, pathIndex2)
    val termru1 = termRightUp(e, loop1, pathIndex1)
    val termru2 = termRightUp(e, loop2, pathIndex2)
    val initru1 = initRightUp(e, loop1, pathIndex1)
    val initru2 = initRightUp(e, loop2, pathIndex2)
    }

    */


}

object SGraph {
    def makeVertices(col: Int, n: Int, m: Int): List[SVertex] = {
            if(n>=m) List[SVertex]()
            else SVertex(col, n) +: makeVertices  (col, n+1, m)
        }
    
    def makeEdgesSCL(initial: SVertex, terminal: SVertex, n: Int, m: Int, typList: List[Int]): List[SEdge] = {
        require((typList.length == m-n), s"The list $typList does not have the required length")
        if(n>=m) List[SEdge]()
        else SEdge(initial, terminal, n,typList.head, 1) +: makeEdgesSCL(initial, terminal, n+1, m, typList.tail)
    }
}





