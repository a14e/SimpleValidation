package a14e.validation.results

import scala.language.implicitConversions


case class ValidationError(text: String)

object ValidationError {
  implicit def from(text: String): ValidationError = ValidationError(text)
}

case class ErrorBuilder[T](build: T => ValidationError) {
  def contramap[B](f: B => T): ErrorBuilder[B] = ErrorBuilder(f andThen build)
}

object ErrorBuilder {
  implicit def from[T](build: T => ValidationError): ErrorBuilder[T] = ErrorBuilder[T](build)

  implicit def fromValidationError[T](err: ValidationError): ErrorBuilder[T] = ErrorBuilder[T](_ => err)

  implicit def fromTextFun[T](build: T => String): ErrorBuilder[T] = ErrorBuilder(x => build(x))

  implicit def fromText[T](text: String): ErrorBuilder[T] = ErrorBuilder[T](_ => ValidationError(text))

}