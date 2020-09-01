package gensm

import scala.math._
import scala.util.parsing.combinator._
import scala.util.Random
import org.jblas.ComplexDouble

class ComplexParse(val variables: Set[String] = Set(),
                    val constants: Map[String, ComplexDouble] = Map(),
                    val unary: Map[String, ComplexDouble => ComplexDouble] = Map(),
                    val binary: Map[String, (ComplexDouble, ComplexDouble) => ComplexDouble] = Map(),
                    val userFcts: Map[String, String => ComplexDouble] = Map()) extends JavaTokenParsers {
    require(constants.keySet.intersect(userFcts.keySet).isEmpty)
    private val allConstants = constants ++ Map("E" -> new ComplexDouble(E), "PI" -> new ComplexDouble(Pi), 
        "Pi" -> new ComplexDouble(Pi))
    // shouldn´t be empty
    private val unaryOps = unary
    private val binaryOps1 = Map[String, (ComplexDouble, ComplexDouble) => ComplexDouble](
        "+" -> (_.add(_)), "-" -> (_.sub(_)), "*" -> (_.mul(_)), "/" -> (_.div(_)), "^" -> (power(_, _))
    )
    private val binaryOps2 = binary

    def power(a: ComplexDouble, b: ComplexDouble): ComplexDouble = {
        val n = round(b.real()).toInt
        def powerHelper(a: ComplexDouble, accum: ComplexDouble, n: Int): ComplexDouble = {
            if(n == 0) accum
            else powerHelper(a, accum.mul(a), n-1)
        } 
        powerHelper(a, new ComplexDouble(1.0), n)
    }

    type Argument = Map[String, ComplexDouble]
    type Formula = Argument => ComplexDouble

    private def fold(d: Formula, l: List[~[String, Formula]]) = l.foldLeft(d) { 
        case (d1, op ~ d2) => arg => binaryOps1(op)(d1(arg), d2(arg))}
    private implicit def set2Parser[V](s: Set[String]) = s.map(_ ^^ identity).reduceLeft(_ | _)
    private implicit def map2Parser[V](m: Map[String, V]) = m.keys.map(_ ^^ identity).reduceLeft(_ | _)
    private def expression: Parser[Formula] = sign ~ term ~ rep(("+" | "-") ~ term) ^^ { 
        case s ~ t ~ l => fold(arg => s .mul(t(arg)), l)}
    private def sign: Parser[ComplexDouble] = opt("+" | "-") ^^ { 
        case None => new ComplexDouble(1.0); case Some("+") => new ComplexDouble(1.0); 
        case Some("-") => new ComplexDouble(-1.0)}
    private def term: Parser[Formula] = longFactor ~ rep(("*" | "/") ~ longFactor) ^^ { 
        case d ~ l => fold(d, l)}
    private def longFactor: Parser[Formula] = shortFactor ~ rep("^" ~ shortFactor) ^^ { 
        case d ~ l => fold(d, l)}
    private def shortFactor: Parser[Formula] = 
        fpn | sign ~ (constant | variable | unaryFct | binaryFct | userFct | "(" ~> expression <~ ")") ^^ {
             case s ~ x => arg => s.mul(x(arg))}
    private def constant: Parser[Formula] = allConstants ^^ (name => arg => allConstants(name))
    private def variable: Parser[Formula] = variables ^^ (name => arg => arg(name))
    private def fpn: Parser[Formula] = floatingPointNumber ^^ (value => arg => new ComplexDouble(value.toDouble))
    private def unaryFct: Parser[Formula] = unaryOps ~ "(" ~ expression ~ ")" ^^ { 
        case op ~ _ ~ d ~ _ => arg => unaryOps(op)(d(arg))}
    private def binaryFct: Parser[Formula] = binaryOps2 ~ "(" ~ expression ~ "," ~ expression ~ ")" ^^ { 
        case op ~ _ ~ d1 ~ _ ~ d2 ~ _ => arg => binaryOps2(op)(d1(arg), d2(arg))}
    private def userFct: Parser[Formula] = userFcts ~ "(" ~ (expression ^^ (_.toString) | ident) <~ ")" ^^ { 
        case fct ~ _ ~ x => arg => userFcts(fct)(x)}
    def evaluate(formula: String) = parseAll(expression, formula).get
}