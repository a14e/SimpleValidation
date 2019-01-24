package a14e.validation.results

import scala.language.implicitConversions


case class TextResult(text: String)

object TextResult {
  implicit def from(text: String): TextResult = TextResult(text)
}

case class TextResultBuilder[T](build: T => TextResult) {
  def contramap[B](f: B => T): TextResultBuilder[B] = TextResultBuilder(f andThen build)
}

object TextResultBuilder {
  implicit def from[T](build: T => TextResult): TextResultBuilder[T] = TextResultBuilder[T](build)

  implicit def fromValidationError[T](err: TextResult): TextResultBuilder[T] = TextResultBuilder[T](_ => err)

  implicit def fromTextFun[T](build: T => String): TextResultBuilder[T] = TextResultBuilder(x => build(x))

  implicit def fromText[T](text: String): TextResultBuilder[T] = TextResultBuilder[T](_ => TextResult(text))

}