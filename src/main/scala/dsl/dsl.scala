package dsl

object dsl {

  trait RuleSym[F[_]] {
    def num(value: Int): F[Int]

    def variable(name: String): F[Int]

    def equal(lhs: F[Int], rhs: F[Int]): F[Boolean]

    def and(lhs: F[Boolean], rhs: F[Boolean]): F[Boolean]
  }

  implicit class RuleSymEqualOps[F[_]](lhs: F[Int])(implicit rs: RuleSym[F]) {
    def EQUALS(rhs: F[Int]): F[Boolean] = rs.equal(lhs, rhs)
  }

  implicit class RuleSymAndOps[F[_]](lhs: F[Boolean])(implicit rs: RuleSym[F]) {
    def AND(rhs: F[Boolean]): F[Boolean] = rs.and(lhs, rhs)
  }

  def NUM[F[_]](value: Int)(implicit rs: RuleSym[F]): F[Int] =
    rs.num(value)

  def VAR[F[_]](name: String)(implicit rs: RuleSym[F]): F[Int] =
    rs.variable(name)

}
