package json

trait Encoder[A] {
  def encode(a: A): Json
}

trait Decoder[A] {
  def decode(json: Json): Either[String, A]
}

trait Json {
  def as[A](implicit d: Decoder[A]): Either[String, A] = d.decode(this)

  override def toString: String = Json.format(0)(this)

}

object Json {
  case class Num(value: Int) extends Json
  case class Str(value: String) extends Json
  case class Bool(value: Boolean) extends Json
  class Obj(val values: Map[String, Json]) extends Json
  class Arr(val values: List[Json]) extends Json

  object Obj {
    def apply(entries: (String, Json)*): Json = new Json.Obj(entries.toMap)

    // private[json] def unapply(json: Json.Obj): Some[Map[String, Json]] = Some(json.values)

    def unapplySeq(json: Json): Option[Seq[(String, Json)]] = json match {
      // case Json.Obj(entries) => Some(entries.toSeq)
      case js: Json.Obj => Some(js.values.toSeq)
      case _            => None
    }
  }

  object Arr {
    def apply[A](as: A*)(implicit enc: Encoder[A]): Json =
      new Json.Arr(as.map(_.asJson).toList)

    // private[json] def unapply(json: Json.Arr): Some[List[Json]] = Some(json.values)

    def unapplySeq(json: Json): Option[Seq[Json]] = json match {
      // case Json.Arr(values) => Some(values.toSeq)
      case js: Json.Arr => Some(js.values.toSeq)
      case _            => None
    }
  }

  def padd(length: Int): String = (0 until length).foldLeft("") {
    case (acc, _) => acc + " "
  }

  def format(padding: Int)(json: Json): String = json match {
    case Json.Num(value)  => value.toString
    case Json.Str(value)  => s""""$value""""
    case Json.Bool(value) => value.toString
    case Json.Obj(values @ _*) =>
      values
        .map { case (key, value) =>
          s"""${padd(padding + 2)}"$key": ${format(padding + 2)(value)}"""
        }
        .mkString("{\n", "\n", s"\n${padd(padding)}}")
    case Json.Arr(values @ _*) =>
      values.map(format(padding + 2)).mkString("[", ", ", "]")
  }

  implicit class EncoderOps[A](a: A)(implicit enc: Encoder[A]) {
    def asJson: Json = enc.encode(a)
  }

  implicit val jsonEncoder = new Encoder[Json] {
    def encode(json: Json): Json = json
  }

  implicit def jsonArrEncoder[A](implicit enc: Encoder[A]): Encoder[List[A]] =
    (as: List[A]) => Json.Arr(as.map(_.asJson))

  implicit val intEncoder: Encoder[Int] =
    (i: Int) => Json.Num(i)

  implicit val strEncoder: Encoder[String] =
    (s: String) => Json.Str(s)

  implicit val boolEncoder: Encoder[Boolean] =
    (b: Boolean) => Json.Bool(b)

  implicit val intDecoder: Decoder[Int] = {
    case Json.Num(i) => Right(i)
    case other       => Left(s"Not a valid number: $other")
  }

  implicit val boolDecoder: Decoder[Boolean] = {
    case Json.Bool(b) => Right(b)
    case other        => Left(s"Not a valid boolean: $other")
  }
}
