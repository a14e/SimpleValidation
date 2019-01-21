package a14e.validation.containers

import scala.concurrent.Future

case class SyncValidationCheck[T, RES](marker: RES,
                                       check: T => Boolean) {

  def contramap[B](f: B => T): SyncValidationCheck[B, RES] = {
    copy(
      check = f andThen check
    )
  }

}

case class AsyncValidationCheck[T, RES](marker: RES,
                                        check: T => Future[Boolean]) {

  def contramap[B](f: B => T): AsyncValidationCheck[B, RES] = {
    copy(
      check = f andThen check
    )
  }

}