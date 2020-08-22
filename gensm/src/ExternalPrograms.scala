package gensm

import Braid._
import SGraph._
import scala.util.Random._
import scala.io.StdIn._
import org.jblas.{ComplexDouble, ComplexDoubleMatrix, Eigen}
import HomologyBasis._

object ExternalPrograms {
    import Braid._
    import HomologyBasis._


    def externalPrograms(args: Array[String]): Unit = {
        println("Please enter the number of strands")
        val strands = readLine().toInt
        println("Please enter the braid description \n (As a sequence of s" +
        "paced integers - like '1 -1 2 3 2')")
        val braid1: String = readLine()
        val p = Braid(braid1.split(" ").map(x => x.toInt).toList, strands)
        println("This braid has the following cycle decomposition:")
        println(p.cycleDecomp)
        println("Please enter color assignments as a sequence of spaced integers \n " +
        "(Starting from 0 and without missing any natural number)")
        val preColList: List[Int] = readLine().split(" ").map(x => x.toInt).toList
        println("Please also enter orientations for the knots as a sequence of spaced integers \n " +
        "(1 is up-down and -1 is down-up)")       
        val orientations = readLine().split(" ").map(x => x.toInt).toList
        val preSgraph = makeGraph(p, preColList, orientations)
        
        /** val hominput = readLine()*/ 
        
        
        /** println("Would you like to find a better order of colors? (Y/N)")
        val findOrder = readLine()*/
        val newColList = findColorPermutations(p, preColList, preColList, preColList.max+1, 50, 
        preSgraph.homologyBasis.length)
        val sgraph = makeGraph(p, newColList, orientations)
        if (sgraph.homologyBasis.isEmpty) println("The Alexander polynomial is 1")
        else {
            
            println("Please enter your desired output format - 1/2/3 = MATLAB/Mathematica/Macaulay2 (no spaces)")
            val choice = readLine() 
            if (choice == "1") {
                val pm = sgraph.presentationMatrix
                
                println("syms " ++ (0.to(orientations.length -1).toList).map(x => 
                "t"++ x.toString).reduceRight((a,b) => a ++ " " ++ b) ++ "\n A = [" ++
                pm.map(x => (x.reduceRight((a,b) => a ++"("++ b ++ ")"))).reduceRight((a,b) => 
                a ++";"++ b) ++ "]")
            }
            else if (choice == "2") {
                val pm = sgraph.presentationMatrix
                println("det [" ++ pm.map(x => ("[" ++ x.reduceRight((a,b) => a ++","++ b) ++ "]")).reduceRight((a,b) => 
                a ++","++ b) ++ "]")
                
            }
            else if (choice == "3") {
                val pm = sgraph.presentationMatrix
                   println("R = ZZ[" ++ (0.to(orientations.length -1).toList).map(x => 
                   "t"++ x.toString).reduceRight((a,b) => a ++ "," ++ b) ++ "]; a = matrix {" ++
                   pm.map(x => ("{" ++ x.reduceRight((a,b) => a ++","++ b) ++ "}")).reduceRight((a,b) => 
                   a ++","++ b) ++ "}")
                   println("Would you also like to get the relevant matrix for signature? (Y/N)")
                   val signatureInput = readLine()
                   if (signatureInput == "Y") {
                       val signatureExtra = (0.to(orientations.length -1).toList).map(x => 
                     "(1-t"++ x.toString ++ "')").reduceRight((a,b) => a ++ "*" ++ b)
                     val sm = pm.map(x => x.map(y => "(" ++ y ++ ")*(" ++ signatureExtra ++ ")"))
                     
                     println("Please enter " ++ (preColList.max +1).toString ++ 
                       " comma-separated input value(s) for the signature")
                       val inputValues = readLine()
                       println(("R = CC[" ++ (0.to(orientations.length -1).toList).map(x => 
                     "t"++ x.toString).reduceRight((a,b) => a ++ "," ++ b) ++ "," ++ (0.to(orientations.length -1).toList).map(x => 
                     "t"++ x.toString ++ "'").reduceRight((a,b) => a ++ "," ++ b) ++ "]; M = matrix {" ++
                     sm.map(x => ("{" ++ x.reduceRight((a,b) => a ++","++ b) ++ "}")).reduceRight((a,b) => 
                     a ++","++ b) ++ "}; ") + "loadPackage \"NumericalAlgebraicGeometry\" ; " +
                     "p = point {{" ++ inputValues ++ "," ++ inputValues ++ "}}; " +
                     "N = evaluate(M, p); " +
                     "eigenvalues N")
                     
                    }
                    else print("")
                    
                }
                else println("That didn't seem right")
            }
            
        }
        
}