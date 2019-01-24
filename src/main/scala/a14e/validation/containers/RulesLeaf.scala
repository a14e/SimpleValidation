package a14e.validation.containers

import a14e.validation.RuleEngine
import a14e.validation.utils.FutureUtils

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal


trait RulesLeaf[T, MARKER] {

  def contramap[B](f: B => T): RulesLeaf[B, MARKER]

  def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): RulesLeaf[T, NEW_MARKER]

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



case class SyncCheckLeaf[T, MARKER](check: SyncRulesCheck[T, MARKER]) extends RulesLeaf[T, MARKER] {

  override def contramap[B](f: B => T): SyncCheckLeaf[B, MARKER] = SyncCheckLeaf(check.contramap(f))

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): SyncCheckLeaf[T, NEW_MARKER] = {
    SyncCheckLeaf(check.copy(marker = f(check.marker)))
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


case class AsyncCheckLeaf[T, MARKER](check: AsyncRulesCheck[T, MARKER]) extends RulesLeaf[T, MARKER] {

  override def contramap[B](f: B => T): AsyncCheckLeaf[B, MARKER] = AsyncCheckLeaf(check.contramap(f))

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): AsyncCheckLeaf[T, NEW_MARKER] = {
    AsyncCheckLeaf(check.copy(marker = f(check.marker)))
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

case class EngineLeaf[T, MARKER](engine: RuleEngine[T,  MARKER]) extends RulesLeaf[T, MARKER] {

  override def contramap[B](f: B => T): EngineLeaf[B, MARKER] = EngineLeaf(engine.contramap(f))

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): RulesLeaf[T, NEW_MARKER] = {
    EngineLeaf(engine.mapMarkers(f))
  }

  override def collectFails(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    engine.collectFails(x)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    engine.firstFail(x)
  }
  override def collectSuccesses(x: T)
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    engine.collectSuccesses(x)
  }

  override def firstSuccess(x: T)
                           (implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {
    engine.firstSuccess(x)
  }
}

case class BuildingEngineLeaf[T, MARKER](build: T => RuleEngine[T, MARKER]) extends RulesLeaf[T, MARKER] {

  override def contramap[B](f: B => T): BuildingEngineLeaf[B, MARKER] = {
    BuildingEngineLeaf { x =>
      build(f(x)).contramap(f)
    }
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): RulesLeaf[T, NEW_MARKER] = {
    BuildingEngineLeaf { x =>
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

case class SeqEngineLeaf[T, ENTRY, MARKER](engine: RuleEngine[ENTRY, MARKER],
                                           readSeq: T => immutable.Seq[ENTRY]) extends RulesLeaf[T, MARKER] {

  override def contramap[B](f: B => T) = {
    SeqEngineLeaf(engine, f andThen readSeq)
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): SeqEngineLeaf[T, ENTRY, NEW_MARKER] = {
    SeqEngineLeaf(engine.mapMarkers(f), readSeq)
  }

  override def collectFails(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    FutureUtils.serially(readSeq(x))(engine.collectFails(_))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recFind(xs: Seq[ENTRY]): Future[Option[MARKER]] = xs match {
      case head +: tail =>
        engine.firstFail(head).flatMap {
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
    FutureUtils.serially(readSeq(x))(engine.collectSuccesses(_))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  override def firstSuccess(x: T)
                           (implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recFind(xs: Seq[ENTRY]): Future[Option[MARKER]] = xs match {
      case head +: tail =>
        engine.firstSuccess(head).flatMap {
          case s@Some(_) => Future.successful(s)
          case _ => recFind(tail)
        }(FutureUtils.sameThreadExecutionContext)
      case _ => Future.successful(None)
    }

    val seq = readSeq(x)
    recFind(seq)
  }

}

case class OptEngineLeaf[T, ENTRY, MARKER](engine: RuleEngine[ENTRY, MARKER],
                                           readOpt: T => Option[ENTRY]) extends RulesLeaf[T, MARKER] {

  override def contramap[B](f: B => T): OptEngineLeaf[B, ENTRY, MARKER] = {
    OptEngineLeaf(engine, f.andThen(readOpt))
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): OptEngineLeaf[T, ENTRY, NEW_MARKER] = {
    OptEngineLeaf(engine.mapMarkers(f), readOpt)
  }

  override def collectFails(in: T)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    readOpt(in) match {
      case Some(value) => engine.collectFails(value)
      case None => Future.successful(Nil)
    }
  }

  override def firstFail(in: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    readOpt(in) match {
      case Some(value) => engine.firstFail(value)
      case None => Future.successful(None)
    }
  }
  override def collectSuccesses(in: T)
                               (implicit
                                executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {


    readOpt(in) match {
      case Some(value) => engine.collectSuccesses(value)
      case None => Future.successful(Nil)
    }
  }

  override def firstSuccess(x: T)(implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {


    readOpt(x) match {
      case Some(value) => engine.firstSuccess(value)
      case None => Future.successful(None)
    }
  }
}

