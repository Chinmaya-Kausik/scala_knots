package gensm


import Braid._
import SGraph._
import scala.util.Random._
import scala.io.StdIn._
import org.jblas.{ComplexDouble, ComplexDoubleMatrix, Eigen}
import HomologyBasis._
import ExternalPrograms._


object GenSM {
    import Braid._
    import HomologyBasis._
    import ExternalPrograms._

    def main(args: Array[String]): Unit = {
        externalPrograms(args)
    }

    
} 
