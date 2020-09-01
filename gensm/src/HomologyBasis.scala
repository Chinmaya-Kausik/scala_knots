package gensm


import Braid._
import SGraph._
import scala.util.Random._
import scala.io.StdIn._

object HomologyBasis {
    import Braid._

    def homBasis(args: Array[String]): Unit = {
        println("\nPlease enter the number of strands")
        val strands = readLine().toInt
        println("\nPlease enter the minimum length of the braid description")
        val braidLength = readLine().toInt
        val p = Braid(makeBraid(braidLength, strands), strands)
        println("\nThe braid description thus generated is")
        println(p.braid)
        println("\nThe cycle decomposition of the random braid thus generated is")
        println(p.cycleDecomp)
        val nKnots = p.ctComponents
        println("\nPlease enter the number of colors")
        val nCol = readLine().toInt
        val colList = randomColorAssignments(nCol, nKnots)
        println("\nThe color assignments thus generated are")
        println(colList)
        val currentLength = makeGraph(p, colList, (0.to(nCol-1).toList).map(x => 1)).homologyBasis.length
        println("\nThe initial length is")
        println(currentLength)
        println("\nPlease enter space separated entries for the numbers of attempts you want tested")
        val tries = readLine().split(" ").map(x => x.toInt)
        println("\nHere are the number of attempts along with the corresponding sizes of the homology bases")
        val tryValues = tries.map(x => 
        (x, colorPermutations(p, colList, nCol, x, currentLength))).map(x => 
        println(x))
    }

    def randomSign(): Int = {
        List(1, -1)(nextInt(2))
    }
    def randomPermutation(lst: List[Int], nCol: Int): List[Int] = {
        val randomColorPermutation = shuffle(0.to(nCol -1).toList)
        lst.map(x => randomColorPermutation(x))
    }
    def makeBraid(braidLength: Int, strands: Int): List[Int] = {
        def braidHelper(braidLength: Int, strands: Int, accum: List[Int]): List[Int] = {
            if (braidLength == 0) accum 
            else braidHelper(braidLength -1, strands, accum :+ (nextInt(strands-1)+1)*randomSign())
        }
        braidHelper(braidLength, strands, List[Int]())
    }
    
    def randomColorAssignments(nCol: Int, nKnots: Int): List[Int] = {
        require(nKnots >= nCol, s"$nKnots knots are not enough for $nCol colors")
        def assignColors(lst: List[Int], tally: List[Int], nCol: Int): (List[Int], List[Int]) = {
            if (nCol == 0) (lst, tally)
            else {
                val randomIndex = tally(nextInt(tally.length))
                assignColors(lst.slice(0, randomIndex) ++ ((nCol-1) +: lst.slice(randomIndex+1, lst.length+1)), 
                    tally.filter(_ != randomIndex), nCol -1)
            }
        }
        def fillColors(lst: List[Int], tally: List[Int], nCol: Int): List[Int] = {
            if(tally.isEmpty) lst
            else {
                val randomColor = shuffle(0.to(nCol-1).toList).head
                fillColors(lst.slice(0, tally.head) ++ (randomColor +: lst.slice(tally.head+1, lst.length +1)),
                    tally.tail, nCol)

            }
        }
        val knots = (0.to(nKnots -1).toList)
        val (lst, tally) = assignColors(knots, knots, nCol)
        fillColors(lst, tally, nCol)
    }

    def colorPermutations(p: Braid, colorAssignments: List[Int], nCol: Int,
        n: Int, currentLength: Int): Int = {
        if (n == 0) currentLength
        else {
            val newColorAssignments = randomPermutation(colorAssignments, nCol)
            val newLength = makeGraph(p, newColorAssignments, 
                (0.to(nCol -1).toList).map(x => 1)).homologyBasis.length
            if(newLength < currentLength) colorPermutations(p, colorAssignments, nCol, n-1, newLength)
            else colorPermutations(p, colorAssignments, nCol, n-1, currentLength)
        }
    }

    def findColorPermutations(p: Braid, colorAssignments: List[Int], revisedColorAssignments: List[Int],
        nCol: Int, n: Int, currentLength: Int): List[Int] = {
        if (n == 0) revisedColorAssignments
        else {
            val newColorAssignments = randomPermutation(colorAssignments, nCol)
            val newLength = makeGraph(p, newColorAssignments, 
                (0.to(nCol -1).toList).map(x => 1)).homologyBasis.length
            if(newLength < currentLength) findColorPermutations(p, colorAssignments, 
                newColorAssignments, nCol, n-1, newLength)
            else findColorPermutations(p, colorAssignments, revisedColorAssignments, nCol, n-1, currentLength)
        }
    }
}