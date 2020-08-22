package gensm

import Braid._
import SGraph._

import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._
import org.jblas.{ComplexDouble, ComplexDoubleMatrix, Eigen}

case class PolyMatrix(ring: MultivariateRing[IntZ], pm: List[List[MultivariatePolynomial[IntZ]]]) {thisMatrix =>

    def detMult(mkk: MultivariatePolynomial[IntZ], mij: MultivariatePolynomial[IntZ], 
        mik: MultivariatePolynomial[IntZ], mkj: MultivariatePolynomial[IntZ], 
        previousPivot: MultivariatePolynomial[IntZ]): MultivariatePolynomial[IntZ] = {
            ring.divideExact(ring.subtract(ring.multiply(mkk, mij), ring.multiply(mik, mkj)), previousPivot)
        }

    def bareissIteration(k: Int, previousPivot: MultivariatePolynomial[IntZ]): PolyMatrix = {
        
        val basis = (0.to(pm.length -1).toList)
        val newPM = basis.map(x=> basis.map(y => (x,y))).map(x => x.map(y =>
            if ((y._1 < k+1) || (y._2 < k)) pm(y._1)(y._2)
            else detMult(pm(k)(k), pm(y._1)(y._2), pm(y._1)(k), pm(k)(y._2), previousPivot)))
        PolyMatrix(ring, newPM)
                
    }

    lazy val bareissDet: MultivariatePolynomial[IntZ] = {
        def bareissDetHelper(k: Int, m: PolyMatrix): MultivariatePolynomial[IntZ] = {
            if (k > m.pm.length -2) m.pm.last.last
            else if (k==0) bareissDetHelper(k+1, m.bareissIteration(k, ring(1)))
            else bareissDetHelper(k+1, m.bareissIteration(k, m.pm(k-1)(k-1)))
        }
        bareissDetHelper(0, thisMatrix)
    }

    lazy val factorizedBareissDet: PolynomialFactorDecomposition[MultivariatePolynomial[IntZ]] = {
        Factor(bareissDet)
    }


    
}

object PolyMatrix {
    def evaluatePoly(r: MultivariatePolynomial[IntZ], values: Array[ComplexDouble]): ComplexDouble = {
        def evaluatePolyHelper(r: MultivariatePolynomial[IntZ], values: Array[ComplexDouble], 
            n: Int): ComplexDouble = {
                ???
            }
        ???
    }
}

/**
import gensm._

 */
