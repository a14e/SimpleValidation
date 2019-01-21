package a14e.validation.containers

import a14e.validation.Validator
import a14e.validation.utils.FutureUtils

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal


trait ValidationContainer[T, MARKER] {

  def contramap[B](f: B => T): ValidationContainer[B, MARKER]

  def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): ValidationContainer[T, NEW_MARKER]

  def collectFails(x: T)
                  (implicit
                   executionContext: ExecutionContext): Future[immutable.Seq[MARKER]]

  def firstFail(x: T)
               (implicit
                executionContext: ExecutionContext): Future[Option[MARKER]]


  def collectSuccesses(x: T)
                      (implicit
                       executionContext: ExecutionContext): Future[immutable.Seq[MARKER]]

  def firstSuccess(x: T)
                  (implicit
                   executionContext: ExecutionContext): Future[Option[MARKER]]
}


case class SyncCheckContainer[T, MARKER](check: SyncValidationCheck[T, MARKER]) extends ValidationContainer[T, MARKER] {

  override def contramap[B](f: B => T): ValidationContainer[B, MARKER] = SyncCheckContainer(check.contramap(f))

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): ValidationContainer[T, NEW_MARKER] = {
    SyncCheckContainer(check.copy(marker = f(check.marker)))
  }

  override def collectFails(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    try {
      if (!check.check(x)) Future.successful(check.marker :: Nil)
      else Future.successful(Nil)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    try {
      if (!check.check(x)) Future.successful(Some(check.marker))
      else Future.successful(None)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }
  override def collectSuccesses(x: T)
                               (implicit
                                executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    try {
      if (check.check(x)) Future.successful(check.marker :: Nil)
      else Future.successful(Nil)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  override def firstSuccess(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[Option[MARKER]] = {
    try {
      if (check.check(x)) Future.successful(Some(check.marker))
      else Future.successful(None)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }
}

case class AsyncCheckContainer[T, MARKER](check: AsyncValidationCheck[T, MARKER]) extends ValidationContainer[T, MARKER] {

  override def contramap[B](f: B => T): ValidationContainer[B, MARKER] = AsyncCheckContainer(check.contramap(f))

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): ValidationContainer[T, NEW_MARKER] = {
    AsyncCheckContainer(check.copy(marker = f(check.marker)))
  }

  override def collectFails(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    check.check(x).map { r =>
      if (!r) check.marker :: Nil
      else Nil
    }(FutureUtils.sameThreadExecutionContext)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    check.check(x).map { r =>
      if (!r) Some(check.marker)
      else None
    }(FutureUtils.sameThreadExecutionContext)
  }
  override def collectSuccesses(x: T)
                               (implicit
                                executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {


    check.check(x).map { r =>
      if (r) check.marker :: Nil
      else Nil
    }(FutureUtils.sameThreadExecutionContext)
  }

  override def firstSuccess(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[Option[MARKER]] = {
    check.check(x).map { r =>
      if (r) Some(check.marker)
      else None
    }(FutureUtils.sameThreadExecutionContext)
  }
}

case class ValidatorContainer[T, MARKER](validator: Validator[T, MARKER]) extends ValidationContainer[T, MARKER] {

  override def contramap[B](f: B => T): ValidationContainer[B, MARKER] = ValidatorContainer(validator.contramap(f))

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): ValidationContainer[T, NEW_MARKER] = {
    ValidatorContainer(validator.mapMarkers(f))
  }

  override def collectFails(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    validator.collectFails(x)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    validator.firstFail(x)
  }
  override def collectSuccesses(x: T)
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    validator.collectSuccesses(x)
  }

  override def firstSuccess(x: T)
                           (implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {
    validator.firstSuccess(x)
  }
}

case class BuildingValidatorContainer[T, MARKER](build: T => Validator[T, MARKER]) extends ValidationContainer[T, MARKER] {
  override def contramap[B](f: B => T): BuildingValidatorContainer[B, MARKER] = BuildingValidatorContainer {
    x =>
      build(f(x)).contramap(f)
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): ValidationContainer[T, NEW_MARKER] = {
    BuildingValidatorContainer { x =>
      build(x).mapMarkers(f)
    }
  }

  override def collectFails(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    build(x).collectFails(x)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    build(x).firstFail(x)
  }
  override def collectSuccesses(x: T)
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    build(x).collectSuccesses(x)
  }

  override def firstSuccess(x: T)(implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {

    build(x).firstSuccess(x)
  }
}

case class MappingValidatorContainer[T, KEY, MARKER](extractKey: T => KEY,
                                                     mapping: Map[KEY, Validator[T, MARKER]]) extends ValidationContainer[T, MARKER] {

  override def contramap[B](f: B => T): MappingValidatorContainer[B, KEY, MARKER] = {

    MappingValidatorContainer(
      f.andThen(extractKey),
      mapping.mapValues(_.contramap(f)).map(identity)
    )
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): MappingValidatorContainer[T, KEY, NEW_MARKER] = {
    MappingValidatorContainer(
      extractKey,
      mapping.mapValues(_.mapMarkers(f)).map(identity)
    )
  }

  override def collectFails(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    resolveValidator(x).collectFails(x)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    resolveValidator(x).firstFail(x)
  }
  override def collectSuccesses(x: T)
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    resolveValidator(x).collectSuccesses(x)
  }

  override def firstSuccess(x: T)(implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {

    resolveValidator(x).firstSuccess(x)
  }

  private def resolveValidator(x: T): Validator[T, MARKER] = {
    val key = extractKey(x)
    mapping.getOrElse(key, Validator.empty)
  }
}

case class SeqValidatorContainer[T, ENTRY, MARKER](validator: Validator[ENTRY, MARKER],
                                                   readSeq: T => immutable.Seq[ENTRY]) extends ValidationContainer[T, MARKER] {

  override def contramap[B](f: B => T): SeqValidatorContainer[B, ENTRY, MARKER] = {
    SeqValidatorContainer(validator, f.andThen(readSeq))
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): SeqValidatorContainer[T, ENTRY, NEW_MARKER] = {
    SeqValidatorContainer(validator.mapMarkers(f), readSeq)
  }

  override def collectFails(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    FutureUtils.serially(readSeq(x))(validator.collectFails(_)).map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recFind(xs: Seq[ENTRY]): Future[Option[MARKER]] = xs match {
      case head +: tail =>
        validator.firstFail(head).flatMap {
          case s@Some(_) => Future.successful(s)
          case _ => recFind(tail)
        }(FutureUtils.sameThreadExecutionContext)
      case _ => Future.successful(None)
    }

    val seq = readSeq(x)
    recFind(seq)
  }

  override def collectSuccesses(x: T)
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    FutureUtils.serially(readSeq(x))(validator.collectSuccesses(_)).map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  override def firstSuccess(x: T)
                           (implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recFind(xs: Seq[ENTRY]): Future[Option[MARKER]] = xs match {
      case head +: tail =>
        validator.firstSuccess(head).flatMap {
          case s@Some(_) => Future.successful(s)
          case _ => recFind(tail)
        }(FutureUtils.sameThreadExecutionContext)
      case _ => Future.successful(None)
    }

    val seq = readSeq(x)
    recFind(seq)
  }

}

case class OptValidatorContainer[T, ENTRY, MARKER](validator: Validator[ENTRY, MARKER],
                                                   readOpt: T => Option[ENTRY]) extends ValidationContainer[T, MARKER] {

  override def contramap[B](f: B => T): OptValidatorContainer[B, ENTRY, MARKER] = {
    OptValidatorContainer(validator, f.andThen(readOpt))
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): OptValidatorContainer[T, ENTRY, NEW_MARKER] = {
    OptValidatorContainer(validator.mapMarkers(f), readOpt)
  }

  override def collectFails(in: T)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    readOpt(in) match {
      case Some(value) => validator.collectFails(value)
      case None => Future.successful(Nil)
    }
  }

  override def firstFail(in: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    readOpt(in) match {
      case Some(value) => validator.firstFail(value)
      case None => Future.successful(None)
    }
  }
  override def collectSuccesses(in: T)
                               (implicit
                                executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {


    readOpt(in) match {
      case Some(value) => validator.collectSuccesses(value)
      case None => Future.successful(Nil)
    }
  }

  override def firstSuccess(x: T)(implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {


    readOpt(x) match {
      case Some(value) => validator.firstSuccess(value)
      case None => Future.successful(None)
    }
  }
}

