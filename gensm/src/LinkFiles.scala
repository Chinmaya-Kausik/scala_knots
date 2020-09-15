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
import PolyMatrix._

import os._

import scala.io.Source._

import java.io.File
import java.io.PrintWriter

object LinkFiles {
    import Braid._
    import HomologyBasis._
    def linkFiles(args: Array[String]): Unit = {
        println("\nPlease enter the full path for the file you want to use" +
          "\nRight-click the file and go to Properties for the path, given under 'Location'" +
          "\nAdd the filename and extension after it and paste here (right-click to paste)")
        val filePath = readLine().trim()
        val braidInputs = fromFile(filePath).mkString.trim().split("\n+").toList.map(x => 
        x.trim().split(",").toList)
        val braids = braidInputs.map(x => Braid(x(1).trim().split(" +").toList.map(x => 
            x.trim().toInt).toList, x(0).toInt))
        println("\nWhich of these do you want? " +
          "\n1. Only the Alexander polynomial \n2. Only signature and nullity values \n3. Both")
        val outputChoice = readLine().trim().toInt
        val preSgraphs = braids.map(x => makeGraph(x, (0.to(x.ctComponents-1)).toList, 
            (0.to(x.ctComponents-1)).toList.map(x => 1)))
        val newColLists = (0.to(braids.length-1).toList).map(x => 
            findColorPermutations(braids(x), (0.to(braids(x).ctComponents-1)).toList, 
            (0.to(braids(x).ctComponents-1)).toList, braids(x).ctComponents, 50, 
            preSgraphs(x).homologyBasis.length))
        val sgraphs = (0.to(braids.length-1).toList).map(x => makeGraph(braids(x), newColLists(x), 
            (0.to(braids(x).ctComponents-1)).toList.map(x => 1)))
        val n = braids.map(x => x.ctComponents).max
        val variables = (0.to(n-1).toArray).map(x => ("t" ++ x.toString)) ++ 
            (0.to(n-1).toArray).map(x => ("tc" ++ x.toString))
        implicit val ring = MultivariateRing(Z, variables)
        val presentationMatrices = sgraphs.map(x => 
            PolyMatrix(ring, x.colSigns.length, x.presentationMatrix.map(y => y.map(z => ring(z)))))
        if (outputChoice == 1) {
            val textValues = "Strands, Braid, Alexander Polynomial, Without Units in the Localized Ring" +:
                braidInputs.zip(presentationMatrices).map(x => if (x._2.pm.isEmpty) 
                List(x._1(0), x._1(1), "1", "1").reduceRight((a,b) => a ++ "," ++ b) else
                List(x._1(0), x._1(1), "\'" ++ x._2.factorizedBareissDet ++ "\'", 
                "\'" ++ ring.stringify(x._2.strippedBareissDet) ++ "\'").reduceRight((a,b) => a ++ "," ++ b))

            val polyFile = new PrintWriter(new File(filePath.slice(0, filePath.lastIndexOf('.')) ++ "Output.csv"))
            polyFile.write(textValues.reduceRight((a,b) => a ++ "\n" ++ b))
            polyFile.close()   
        }
        else if (outputChoice == 2) {
            println("\nThe default values are -1. Please specify if you would like to use that (Y/N).")
            val defaultUse = readLine()
            println(s"\nPlease enter any other sets of $n space-separated input angle(s) " +
              s"you want us to evaluate the signature and nullity for" +
              s", in order (values must be specified in terms fractions of 2*Pi) ~\n" +
            s"Separate different sets by commas \n" +
            s"Leave this blank if you only want to use the default\n" +
            s"For example: \'1/2 1/5, 1/8 1/16\' = Pi 2*Pi/5, Pi/4 Pi/8")
            val doubleParser = new DoubleParse(unary = Map("sin" -> sin, "cos" -> cos))
            if (defaultUse == "Y") {
                val defaultValues = (0.to(n-1)).toList.map(x => Pi)
                val userValues = readLine()
                val inputValues = if (userValues.isEmpty) List(defaultValues) else 
                    (userValues.trim().split(",").toList.map(x => x.trim().split(" ").toList.map(y => 
                    doubleParser.evaluate(y.trim())(Map())*2*Pi).toList) :+ defaultValues)
            
                val textValues = ("Strands, Braids" ++ ("1/2 "*n) ++ userValues) +: 
                    braidInputs.zip(presentationMatrices).map(x => 
                    if (x._2.pm.isEmpty) (x._1 ++ 
                    inputValues.map(x => "0; 0")).reduceRight((a,b) => a ++ "," ++ b) 
                    else (x._1 ++ inputValues.map(y => x._2.signatureFunction(y).toString ++ "; " ++ 
                    x._2.nullityFunction(y).toString)).reduceRight((a,b) => a ++ "," ++ b))
                val polyFile = new PrintWriter(new File(
                    filePath.slice(0, filePath.lastIndexOf('.')) ++ "Output.csv"))
                polyFile.write(textValues.reduceRight((a,b) => a ++ "\n" ++ b))
                polyFile.close()
            }
            else {
                val userValues = readLine()
                val inputValues = userValues.trim().split(",").toList.map(x => 
                    x.trim().split(" ").toList.map(y => doubleParser.evaluate(y.trim())(Map())*2*Pi).toList)
                val textValues = ("Strands, Braids, " ++ userValues) +: 
                    braidInputs.zip(presentationMatrices).map(x => 
                    if (x._2.pm.isEmpty) (x._1 ++ 
                    inputValues.map(x => "0; 0")).reduceRight((a,b) => a ++ "," ++ b) 
                    else (x._1 ++ inputValues.map(y => x._2.signatureFunction(y).toString ++ "; " ++ 
                    x._2.nullityFunction(y).toString)).reduceRight((a,b) => a ++ "," ++ b))
                val polyFile = new PrintWriter(new File(
                    filePath.slice(0, filePath.lastIndexOf('.')) ++ "Output.csv"))
                polyFile.write(textValues.reduceRight((a,b) => a ++ "\n" ++ b))
                polyFile.close()
            }
            
        }


        else if (outputChoice == 3) {
            println("\nThe default values are -1. Please specify if you would like to use that (Y/N).")
            val defaultUse = readLine()
            println(s"\nPlease enter any other sets of $n space-separated input angle(s) " +
              s"you want us to evaluate the signature for" +
             s", in order (values must be specified in terms fractions of 2*Pi) ~\n" +
            s"Separate different sets by commas \n" +
            s"Leave this blank if you only want to use the default\n" +
            s"For example: \'1/2 1/5, 1/8 1/16\' = Pi 2*Pi/5, Pi/4 Pi/8")
            val doubleParser = new DoubleParse(unary = Map("sin" -> sin, "cos" -> cos))
            if (defaultUse == "Y") {
                val defaultValues = (0.to(n-1)).toList.map(x => Pi)
                val userValues = readLine()
                val inputValues = if (userValues.isEmpty) List(defaultValues) else 
                    (userValues.trim().split(",").toList.map(x => x.trim().split(" ").toList.map(y => 
                    doubleParser.evaluate(y.trim())(Map())*2*Pi).toList) :+ defaultValues)
            
                val textValues = ("Strands, Braids, Alexander Polynomial, " ++
                    "Without Units in the Localized Ring, " ++ ("1/2 "*n) ++ "," ++ userValues) +: 
                    braidInputs.zip(presentationMatrices).map(x => 
                    if (x._2.pm.isEmpty) (x._1 ++ List("\'1\'", "\'1\'") ++ 
                        inputValues.map(x => "0; 0")).reduceRight((a,b) => a ++ "," ++ b) 
                    else (x._1 ++ List("\'" ++ x._2.factorizedBareissDet++ "\'", 
                        "\'" ++ ring.stringify(x._2.strippedBareissDet) ++ "\'") ++ inputValues.map(y => 
                        x._2.signatureFunction(y).toString ++ "; " ++ 
                        x._2.nullityFunction(y).toString)).reduceRight((a,b) => a ++ "," ++ b))
                val polyFile = new PrintWriter(new File(
                    filePath.slice(0, filePath.lastIndexOf('.')) ++ "Output.csv"))
                polyFile.write(textValues.reduceRight((a,b) => a ++ "\n" ++ b))
                polyFile.close()
            }
            else {
                val userValues = readLine()
                val inputValues = userValues.trim().split(",").toList.map(x => 
                    x.trim().split(" ").toList.map(y => doubleParser.evaluate(y.trim())(Map())*2*Pi).toList)
                val textValues = ("Strands, Braids, Alexander Polynomial, " +
                    "Without Units in the Localized Ring, " ++ userValues) +: 
                    braidInputs.zip(presentationMatrices).map(x => 
                    if (x._2.pm.isEmpty) (x._1 ++ List("\'1\'", "\'1\'") ++ 
                        inputValues.map(x => "0; 0")).reduceRight((a,b) => a ++ "," ++ b) 
                    else (x._1 ++ List("\'" ++ x._2.factorizedBareissDet++ "\'", 
                        "\'" ++ ring.stringify(x._2.strippedBareissDet) ++ "\'") ++ inputValues.map(y => 
                        x._2.signatureFunction(y).toString ++ "; " ++ 
                        x._2.nullityFunction(y).toString)).reduceRight((a,b) => a ++ "," ++ b))
                val polyFile = new PrintWriter(new File(
                    filePath.slice(0, filePath.lastIndexOf('.')) ++ "Output.csv"))
                polyFile.write(textValues.reduceRight((a,b) => a ++ "\n" ++ b))
                polyFile.close()
            }
        }
        else println("\nThat didn't seem right")
    }
}
