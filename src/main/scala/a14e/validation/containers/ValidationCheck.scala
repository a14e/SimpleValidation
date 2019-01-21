package a14e.validation.containers

import scala.concurrent.Future

case class ValidationCheck[T, RES](marker: RES,
                                   check: T => Future[Boolean]) {

  def contramap[B](f: B => T): ValidationCheck[B, RES] = {
    copy(
      check = f andThen check
    )
  }

}