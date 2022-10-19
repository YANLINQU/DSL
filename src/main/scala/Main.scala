import com.typesafe.config._
import dsl.dsl._
import pureconfig.ConfigReader.Result
import pureconfig._

import scala.util._

object Main extends App {

  type Rule[F[_], A] = RuleSym[F] => F[A]
  type Pretty[_] = String

  val prettyInterpreter = new RuleSym[Pretty] {
    override def num(value: Int): Pretty[Int] = value.toString

    override def variable(name: String): Pretty[Int] = s"$$$name"

    override def equal(lhs: Pretty[Int], rhs: Pretty[Int]): Pretty[Boolean] =
      s"($lhs EQUALS $rhs)"

    override def and(
        lhs: Pretty[Boolean],
        rhs: Pretty[Boolean]
    ): Pretty[Boolean] = s"$lhs AND $rhs"
  }

  def readerReadsMissingKeys[A](
      cursor: ConfigObjectCursor,
      key: String,
      reader: ConfigReader[A]
  ): Result[A] = {
    reader match {
      case r: ReadsMissingKeys => r.from(cursor.atKeyOrUndefined(key))
      case r                   => cursor.atKey(key).flatMap(r.from)
    }
  }

  def readerPartial1[B, A](
      cursor: ConfigObjectCursor,
      f: PartialFunction[A, B]
  )(a: A): Result[B] =
    Try(f(a)) match {
      case Failure(exception) => cursor.failed[B](error.ExceptionThrown(exception))
      case Success(value) => Right(value)
    }

  def readerForObj1Partial[B, A](key: String)(
      f: PartialFunction[A, B]
  )(implicit reader: ConfigReader[A]): ConfigReader[B] =
    (cur: ConfigCursor) =>
      cur.asObjectCursor.flatMap { cursor =>
        readerReadsMissingKeys(cursor, key, reader).flatMap(
          readerPartial1(cursor, f)
        )
      }

  implicit def ruleIntConfigReader[F[_]]: ConfigReader[Rule[F, Int]] = {
    val numConfigReader: ConfigReader[Rule[F, Int]] =
      ConfigReader.forProduct1[Rule[F, Int], Int]("num")(value =>
        (rs: RuleSym[F]) => rs.num(value)
      )

    val variableConfigReader: ConfigReader[Rule[F, Int]] =
      ConfigReader.forProduct1[Rule[F, Int], String]("variable")(value =>
        (rs: RuleSym[F]) => rs.variable(value)
      )

    numConfigReader orElse variableConfigReader
  }

  def andDecode[F[_]]: ConfigReader[Rule[F, Boolean]] =
    readerForObj1Partial[Rule[F, Boolean], List[Rule[F, Boolean]]]("and") {
      case left :: tail =>
        (rs: RuleSym[F]) =>
          tail.foldLeft(left(rs)) {
            case (acc, right) => rs.and(acc, right(rs))
          }
    }

  implicit def ruleBoolConfDecoder[F[_]]: ConfigReader[Rule[F, Boolean]] = {
    def equalsDecode[F[_]]: ConfigReader[Rule[F, Boolean]] =
      readerForObj1Partial[Rule[F, Boolean], List[Rule[F, Int]]]("equal") {
        case left :: right :: Nil =>
          (rs: RuleSym[F]) => rs.equal(left(rs), right(rs))
      }

    equalsDecode[F] orElse andDecode[F]
  }

  val config: Config = ConfigFactory.load()

  def loadConfig[F[_]](
      ruleSym: RuleSym[F]
  )(config: Config): Result[F[Boolean]] =
    ConfigSource
      .fromConfig(config)
      .load[Rule[F, Boolean]]
      .map(_(ruleSym))

  println(loadConfig(prettyInterpreter)(config))

}
