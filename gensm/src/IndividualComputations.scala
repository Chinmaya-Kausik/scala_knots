package gensm

import cc.redberry.rings

import rings.poly.PolynomialMethods._
import scala.math._
import rings.scaladsl._
import syntax._
import Braid._
import SGraph._
import scala.util.Random._
import scala.io.StdIn._
import org.jblas.{ComplexDouble, ComplexDoubleMatrix, Eigen}
import org.jblas.ComplexDouble._ 
import HomologyBasis._
import ExternalPrograms._
import ComplexParse._
import DoubleParse._


object IndividualComputations {
    def individualComputations(args: Array[String]): Unit = {
        println("\nPlease enter the number of strands")
        val strands = readLine().toInt
        println("\nPlease enter the braid description \n (As a sequence of s" +
        "paced integers - like '1 -1 2 3 2')")
        val braid1: String = readLine()
        val p = Braid(braid1.split(" ").map(x => x.toInt).toList, strands)
        println("\nThis braid has the following cycle decomposition:")
        println(p.cycleDecomp)
        println("\nPlease enter color assignments as a sequence of spaced integers \n " +
        "(Starting from 0 and without missing any natural number)")
        val preColList: List[Int] = readLine().split(" ").map(x => x.toInt).toList
        println("\nPlease also enter orientations for the knots as a sequence of spaced integers \n " +
        "(1 is up-down and -1 is down-up)")       
        val orientations = readLine().split(" ").map(x => x.toInt).toList
        val preSgraph = makeGraph(p, preColList, orientations)
        
        val newColList = findColorPermutations(p, preColList, preColList, preColList.max+1, 50, 
        preSgraph.homologyBasis.length)
        val sgraph = makeGraph(p, newColList, orientations)
        if (sgraph.homologyBasis.isEmpty) println("\nThe Alexander polynomial is 1")

        else {
            val pm = sgraph.presentationMatrix
            val variables = ((0.to(orientations.length -1).toList).map(x => "t"++ x.toString) ++ 
            (0.to(orientations.length -1).toList).map(x => "tc"++ x.toString)).toArray
            val ring = MultivariateRing(Z, variables)
            val m = PolyMatrix(ring, orientations.length, pm.map(x => x.map(y => ring(y))))
            println("\nThe Alexander polynomial is") 
            println(m.factorizedBareissDet)
            println("\nWould you also like to evaluate the signature and nullity at a point? (Y/N)")
            val signatureInput = readLine()
            if (signatureInput == "Y") { 
                println("\nPlease enter " ++ (preColList.max +1).toString ++ 
                " space-separated input angle(s) for the signature in order " +
                "(values must be in terms of fractions of 2*Pi) ~\n" +
                "For example: \'1/2 1/5\' = Pi 2*Pi/5")
                val doubleParser = new DoubleParse(unary = Map("sin" -> sin, "cos" -> cos))
                val inputValues = readLine().trim().split(" ").map(y => 
                doubleParser.evaluate(y.trim())(Map())*2*Pi).toList
                val signature = m.signatureFunction(inputValues)
                val nullity = m.nullityFunction(inputValues)
                println(s"\nThe signature is $signature\n The nullity is $nullity")
            }
            else println("")
        }
    }
}