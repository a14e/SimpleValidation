package a14e.validation.containers

import scala.concurrent.Future

case class SyncRulesCheck[T, RES](marker: RES,
                                  check: T => Boolean) {
  def contramap[B](f: B => T): SyncRulesCheck[B, RES] = SyncRulesCheck(marker, f andThen check)
}

case class AsyncRulesCheck[T, RES](marker: RES,
                                   check: T => Future[Boolean]) {

  def contramap[B](f: B => T): AsyncRulesCheck[B, RES] = AsyncRulesCheck(marker, f andThen check)
}