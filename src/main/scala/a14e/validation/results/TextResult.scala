package a14e.validation.results

import scala.language.implicitConversions


case class TextResult(text: String)

object TextResult {
  implicit def from(text: String): TextResult = TextResult(text)
}
