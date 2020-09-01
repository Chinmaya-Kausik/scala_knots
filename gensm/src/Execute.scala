package gensm


import Braid._
import SGraph._
import scala.util.Random._
import scala.io.StdIn._
import org.jblas.{ComplexDouble, ComplexDoubleMatrix, Eigen}
import HomologyBasis._
import ExternalPrograms._
import IndividualComputations._
import LinkFiles._


object GenSM {
    import Braid._
    import HomologyBasis._            
    import ExternalPrograms._
    import IndividualComputations._
    import LinkFiles._
    val choiceList = List(individualComputations _, linkFiles _, externalPrograms _, homBasis _)

    def main(args: Array[String]): Unit = {
        println("Welcome to GenSM! Please choose the task you would like to complete:" +
          "\n1. Perform computations for links one by one." +
          "\n2. Request outputs for links from a given file." +
          "\n3. Get code for individual links to paste into external prorgams like MATLAB/Mathematica/Macaulay2." +
          "\n4. Play around with the genera of (reasonably) optimized C-Complexes for random braids" +
          "\n5. Exit")
        val choice = readLine().toInt
        loopMain(choice, args)
    }

    def loop(input: String, args: Array[String], f: Array[String] => Unit): Unit = {
        if (input == "N") println("")
        else {
            f(args)
            println("\nDo you want to do this again? (Y/N)")
            val newInput = readLine()
            loop(newInput, args, f)
        }
    }

    def loopMain(choice: Int, args: Array[String]): Unit = {
        if (choice == 5) println("")
        else {
            loop("Y", args, choiceList(choice -1))
            println("Choose the task you would like to complete:" +
              "\n1. Individual Computations" +
              "\n2. Computations for Files" +
              "\n3. External Programs" +
              "\n4. Genera for Random C-Complexes" +
              "\n5. Exit")
            val newChoice = readLine().toInt
            loopMain(newChoice, args)
        } 
    }
    
} 
