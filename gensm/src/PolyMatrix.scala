package gensm

import Braid._
import SGraph._

import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._

import scala.math._

import org.jblas.{ComplexDouble, ComplexDoubleMatrix, Eigen}

import ComplexParse._

import org.ojalgo.matrix._
import org.ojalgo.scalar.ComplexNumber._
import org.ojalgo.matrix.decomposition._
import org.ojalgo.array._
import org.ojalgo.structure._

case class PolyMatrix(ring: MultivariateRing[IntZ], varNum: Int, pm: List[List[MultivariatePolynomial[IntZ]]]) {thisMatrix =>

    val basis = (0.to(pm.length -1).toList)
    val indexMatrix = basis.map(x=> basis.map(y => (x,y)))

    def detMult(mkk: MultivariatePolynomial[IntZ], mij: MultivariatePolynomial[IntZ], 
        mik: MultivariatePolynomial[IntZ], mkj: MultivariatePolynomial[IntZ], 
        previousPivot: MultivariatePolynomial[IntZ]): MultivariatePolynomial[IntZ] = {
            ring.divideExact(ring.subtract(ring.multiply(mkk, mij), ring.multiply(mik, mkj)), previousPivot)
    }

    def bareissIteration(k: Int, previousPivot: MultivariatePolynomial[IntZ]): PolyMatrix = {
        if(pm(k)(k) == ring(0)) {
            val (i, singular) = searchPivot(k)
            if (singular == true) thisMatrix
            else {
                val modifiedPM = switchColumns(k,i)
                val newPM = indexMatrix.map(x => x.map(y =>
                    if ((y._1 < k+1) || (y._2 < k)) modifiedPM(y._1)(y._2)
                    else detMult(modifiedPM(k)(k), modifiedPM(y._1)(y._2), modifiedPM(y._1)(k), 
                        modifiedPM(k)(y._2), previousPivot)))
                PolyMatrix(ring, varNum, newPM)
            }
        }
        else {
            val newPM = indexMatrix.map(x => x.map(y =>
                if ((y._1 < k+1) || (y._2 < k)) pm(y._1)(y._2)
                else detMult(pm(k)(k), pm(y._1)(y._2), pm(y._1)(k), pm(k)(y._2), previousPivot)))
            PolyMatrix(ring, varNum, newPM)
        }
        
                
    }

    lazy val bareissDet: MultivariatePolynomial[IntZ] = {
        def bareissDetHelper(k: Int, m: PolyMatrix): MultivariatePolynomial[IntZ] = {
            if (k > m.pm.length -2) m.pm.last.last
            else if (k==0) {
                val nextMatrix = m.bareissIteration(k, ring(1)) 
                if (nextMatrix.pm(k)(k) == ring(0)) ring(0)
                else bareissDetHelper(k+1, nextMatrix)
            }
            else {
                val nextMatrix = m.bareissIteration(k, m.pm(k-1)(k-1)) 
                if (nextMatrix.pm(k)(k) == ring(0)) ring(0)
                else bareissDetHelper(k+1, nextMatrix)
            }
        }
        bareissDetHelper(0, thisMatrix)
    }

    def switchColumns(i: Int, j: Int): List[List[MultivariatePolynomial[IntZ]]] = basis.map(x => 
            basis.map(y => if (y == i) pm(x)(j) else {
                if (y == j) pm(x)(i) else pm(x)(y)}))


    def searchPivot(i: Int): (Int, Boolean) = {
        def searchPivotHelper(i: Int, j: Int): (Int, Boolean) = {
            if (pm(i)(j) != ring(0)) (j, false)
            else if (j == pm.length) (j, true)
            else searchPivotHelper(i, j+1)
        }
        searchPivotHelper(i, i)
    }

    lazy val factorizedBareissDet: String = {
        if (bareissDet == ring(0)) "0"
        else ring.stringify(Factor(bareissDet))
    }

    lazy val strippedBareissDet: MultivariatePolynomial[IntZ] = {
        val n = ring.nVariables()/2
        def remFactors(r: MultivariatePolynomial[IntZ], 
            s: MultivariatePolynomial[IntZ]): MultivariatePolynomial[IntZ] = {
                if (r == ring(0)) r
                else if (ring.remainder(r, s) == ring(0)) remFactors(ring.divideExact(r,s), s)
                else r
            }
        def originalUnits(k: Int, m: Int, r: MultivariatePolynomial[IntZ]): MultivariatePolynomial[IntZ] = {
            if (m > k) r
            else originalUnits(k, m+1, remFactors(r, ring("t"++ m.toString)))
        }
        def localizedUnits(k: Int, m: Int, r: MultivariatePolynomial[IntZ]): MultivariatePolynomial[IntZ] = {
            if (m > k) r
            else localizedUnits(k, m+1, remFactors(r, ring("t"++ m.toString ++ "-1")))
        }
        originalUnits(n-1, 0, localizedUnits(n-1, 0, bareissDet))
    }

    def evaluate(values: List[ComplexDouble]): ComplexDoubleMatrix = {
        require(values.length >= varNum, s"$values does not have enough values")
        val variables = (0.to(values.length -1).toArray)
        val maps = variables.map(x => Map(("t" ++ x.toString) -> values(x))) ++ 
            variables.map(x => Map(("tc" ++ x.toString) -> values(x).conj()))
        val f: Map[String, ComplexDouble] = maps.reduce((a,b) => (a ++ b))
        val formulaParser = new ComplexParse(constants = f)
        val complexArray = pm.map(x => 
            x.map(y => formulaParser.evaluate(ring.stringify(y))(Map())).toArray).toArray
        val complexArrayColumns = indexMatrix.toArray.map(x => x.toArray.map(y => 
            complexArray(y._2)(y._1))).map(x => new ComplexDoubleMatrix(x))
        complexArrayColumns.reduce((a,b) => ComplexDoubleMatrix.concatHorizontally(a,b))
        
    }

    def signatureFunction(inputValues: List[Double]): Int = {
        val signatureExtra = ring((0.to(varNum-1).toList).map(x => 
                     "(1-tc"++ x.toString ++ ")").reduceRight((a,b) => a ++ "*" ++ b))
        val sm = pm.map(x => x.map(y => ring.multiply(signatureExtra, y)))
        val evaluatedSM = PolyMatrix(ring, varNum, sm).evaluate(inputValues.toList.map(x => 
            new ComplexDouble(cos(x), sin(x)))).toArray2   
        val ojAlgoSM = ComplexMatrix.FACTORY.makeWrapper(Access2D.wrap( 
            evaluatedSM.map(x => x.map(y => of(y.real, y.imag)))))
        require(ojAlgoSM.isHermitian(), s"The matrix $ojAlgoSM is not Hermitian")    
        val eigen = Eigenvalue.make(ojAlgoSM, true)
        eigen.decompose(ojAlgoSM)
        val eigenvaluesArray1D = eigen.getEigenvalues()
        val eigenvalues = (0.to(eigenvaluesArray1D.length.toInt -1).toList).map(x => 
            eigenvaluesArray1D.get(x).getReal())
        eigenvalues.count(x => (x> 0.0)) - eigenvalues.count(x => (x< 0.0))
    }
        
        
        
    }

object PolyMatrix {
    def evaluatePoly(ring: MultivariateRing[IntZ], r: MultivariatePolynomial[IntZ], values: List[ComplexDouble]): ComplexDouble = {
        val variables = (0.to(values.length -1).toList)
        val maps = variables.map(x => Map(("t" ++ x.toString) -> values(x))) ++ 
            variables.map(x => Map(("tc" ++ x.toString) -> values(x).conj()))
        val f: Map[String, ComplexDouble] = maps.reduce((a,b) => (a ++ b))
        val formulaParser = new ComplexParse(constants = f)
        formulaParser.evaluate(ring.stringify(r))(Map())
    }
}

/**
{
import gensm._
import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._

import org.jblas.{ComplexDouble, ComplexDoubleMatrix, Eigen}
import org.jblas.ComplexDouble._
import scala.math._

import ComplexParse._

import Braid._
import SGraph._

import org.ojalgo.matrix._
import org.ojalgo.scalar.ComplexNumber._
import org.ojalgo.matrix.decomposition._
import org.ojalgo.array._
import org.ojalgo.structure._

val orientations = List(1,2,3)
val variables = ((0.to(orientations.length -1).toList).map(x => "t"++ x.toString) ++
              (0.to(orientations.length -1).toList).map(x => "tc"++ x.toString)).toArray
implicit val ring = MultivariateRing(Z, variables)   
val p = Braid(List(1,1,1,2,2),3)
val sgraph = makeGraph(p, List(0,1), List(1,1))
val pm = sgraph.presentationMatrix.map(x => x.map(y => ring(y)))
val doubleParser = new DoubleParse(unary = Map("sin" -> sin, "cos" -> cos))

val signatureExtra = ring((0.to(orientations.length-1).toList).map(x => 
                     "(1-tc"++ x.toString ++ ")").reduceRight((a,b) => a ++ "*" ++ b))
val sm = pm.map(x => x.map(y => ring.multiply(signatureExtra, y)))
}

val inputValues = readLine().split(";").map(x => x.split(",").map(y => 
    doubleParser.evaluate(y.trim())(Map()))).map(x => new ComplexDouble(x(0), x(1))).toList

    1, sqrt(3)/2; sin(Pi/5)
 */

