package a14e.validation.containers

import a14e.validation.Validator
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}


trait ValidationContainer[T, MARKER] {

  def contramap[B](f: B => T): ValidationContainer[B, MARKER]

  def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): ValidationContainer[T, NEW_MARKER]

  def collectFails(x: T)
                  (implicit
                   executionContext: ExecutionContext,
                   materializer: Materializer): Future[immutable.Seq[MARKER]]

  def firstFail(x: T)
               (implicit
                executionContext: ExecutionContext): Future[Option[MARKER]]


  def collectSuccesses(x: T)
                      (implicit
                       executionContext: ExecutionContext,
                       materializer: Materializer): Future[immutable.Seq[MARKER]]

  def firstSuccess(x: T)
                  (implicit
                   executionContext: ExecutionContext): Future[Option[MARKER]]
}

case class CheckContainer[T, MARKER](check: ValidationCheck[T, MARKER]) extends ValidationContainer[T, MARKER] {

  override def contramap[B](f: B => T): ValidationContainer[B, MARKER] = CheckContainer(check.contramap(f))

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): ValidationContainer[T, NEW_MARKER] = {
    CheckContainer(check.copy(marker = f(check.marker)))
  }

  override def collectFails(x: T)
                           (implicit
                            executionContext: ExecutionContext,
                            materializer: Materializer): Future[immutable.Seq[MARKER]] = {

    check.check(x).map { r =>
      if (!r) check.marker :: Nil
      else Nil
    }
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    check.check(x).map { r =>
      if (!r) Some(check.marker)
      else None
    }
  }
  override def collectSuccesses(x: T)
                               (implicit executionContext: ExecutionContext,
                                materializer: Materializer): Future[immutable.Seq[MARKER]] = {


    check.check(x).map { r =>
      if (r) check.marker :: Nil
      else Nil
    }
  }

  override def firstSuccess(x: T)(implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {
    check.check(x).map { r =>
      if (r) Some(check.marker)
      else None
    }
  }
}

case class ValidatorContainer[T, MARKER](validator: Validator[T, MARKER]) extends ValidationContainer[T, MARKER] {
  override def contramap[B](f: B => T): ValidationContainer[B, MARKER] = ValidatorContainer(validator.contramap(f))

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): ValidationContainer[T, NEW_MARKER] = {
    ValidatorContainer(validator.mapMarkers(f))
  }

  override def collectFails(x: T)
                           (implicit
                            executionContext: ExecutionContext,
                            materializer: Materializer): Future[immutable.Seq[MARKER]] = {
    validator.collectFails(x)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    validator.firstFail(x)
  }
  override def collectSuccesses(x: T)
                               (implicit executionContext: ExecutionContext,
                                materializer: Materializer): Future[immutable.Seq[MARKER]] = {

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
                            executionContext: ExecutionContext,
                            materializer: Materializer): Future[immutable.Seq[MARKER]] = {
    build(x).collectFails(x)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    build(x).firstFail(x)
  }
  override def collectSuccesses(x: T)
                               (implicit executionContext: ExecutionContext,
                                materializer: Materializer): Future[immutable.Seq[MARKER]] = {
    build(x).collectSuccesses(x)
  }

  override def firstSuccess(x: T)(implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {

    build(x).firstSuccess(x)
  }
}

case class SeqValidatorContainer[T, ENTRY, MARKER](validator: Validator[ENTRY, MARKER],
                                                   readSeq: T => immutable.Seq[ENTRY]) extends ValidationContainer[T, MARKER] {

  override def contramap[B](f: B => T): SeqValidatorContainer[B, ENTRY, MARKER] = {
    SeqValidatorContainer(validator, f andThen readSeq)
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): SeqValidatorContainer[T, ENTRY, NEW_MARKER] = {
    SeqValidatorContainer(validator.mapMarkers(f), readSeq)
  }

  override def collectFails(x: T)
                           (implicit
                            executionContext: ExecutionContext,
                            materializer: Materializer): Future[immutable.Seq[MARKER]] = {
    Source(readSeq(x))
      .mapAsync(1)(validator.collectFails(_))
      .mapConcat(x => x)
      .runWith(Sink.seq)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recFind(xs: Seq[ENTRY]): Future[Option[MARKER]] = xs match {
      case head +: tail =>
        validator.firstFail(head).flatMap {
          case s@Some(_) => Future.successful(s)
          case _ => recFind(tail)
        }
      case _ => Future.successful(None)
    }

    val seq = readSeq(x)
    recFind(seq)
  }
  override def collectSuccesses(x: T)
                               (implicit executionContext: ExecutionContext,
                                materializer: Materializer): Future[immutable.Seq[MARKER]] = {
    Source(readSeq(x))
      .mapAsync(1)(validator.collectSuccesses(_))
      .mapConcat(x => x)
      .runWith(Sink.seq)
  }

  override def firstSuccess(x: T)
                           (implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recFind(xs: Seq[ENTRY]): Future[Option[MARKER]] = xs match {
      case head +: tail =>
        validator.firstSuccess(head).flatMap {
          case s@Some(_) => Future.successful(s)
          case _ => recFind(tail)
        }
      case _ => Future.successful(None)
    }

    val seq = readSeq(x)
    recFind(seq)
  }

}

case class OptValidatorContainer[T, ENTRY, MARKER](validator: Validator[ENTRY, MARKER],
                                                   readOpt: T => Option[ENTRY]) extends ValidationContainer[T, MARKER] {

  override def contramap[B](f: B => T): OptValidatorContainer[B, ENTRY, MARKER] = {
    OptValidatorContainer(validator, f andThen readOpt)
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): OptValidatorContainer[T, ENTRY, NEW_MARKER] = {
    OptValidatorContainer(validator.mapMarkers(f), readOpt)
  }

  override def collectFails(in: T)
                           (implicit
                            executionContext: ExecutionContext,
                            materializer: Materializer): Future[immutable.Seq[MARKER]] = {

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
                                executionContext: ExecutionContext,
                                materializer: Materializer): Future[immutable.Seq[MARKER]] = {


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
