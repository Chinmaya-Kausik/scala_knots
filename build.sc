import mill._, scalalib._

object gensm extends ScalaModule {
  def scalaVersion = "2.12.10"
  def ivyDeps = Agg(ivy"cc.redberry::rings.scaladsl:2.5.7", ivy"org.jblas:jblas:1.2.4", ivy"org.scala-lang.modules::scala-parser-combinators:1.1.2")
}