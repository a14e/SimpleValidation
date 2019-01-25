package a14e.validation.containers

import a14e.validation.RuleEngine
import a14e.validation.utils.FutureUtils

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal


trait RulesNode[T, MARKER] {

  def contramap[B](f: B => T): RulesNode[B, MARKER]

  def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): RulesNode[T, NEW_MARKER]

  def collectFails(x: T,
                   parallelLevel: Int = 1)
                  (implicit
                   executionContext: ExecutionContext): Future[immutable.Seq[MARKER]]

  def firstFail(x: T)
               (implicit
                executionContext: ExecutionContext): Future[Option[MARKER]]


  def collectSuccesses(x: T,
                       parallelLevel: Int = 1)
                      (implicit
                       executionContext: ExecutionContext): Future[immutable.Seq[MARKER]]

  def firstSuccess(x: T)
                  (implicit
                   executionContext: ExecutionContext): Future[Option[MARKER]]
}



case class SyncCheckNode[T, MARKER](marker: MARKER,
                                    check: T => Boolean) extends RulesNode[T, MARKER] {

  override def contramap[B](f: B => T): SyncCheckNode[B, MARKER] = this.copy(check = f andThen this.check)

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): SyncCheckNode[T, NEW_MARKER] = {
    this.copy(marker = f(marker))
  }

  override def collectFails(x: T,
                            parallelLevel: Int = 1)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    try {
      if (!check(x)) Future.successful(marker :: Nil)
      else Future.successful(Nil)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    try {
      if (!check(x)) Future.successful(Some(marker))
      else Future.successful(None)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }
  override def collectSuccesses(x: T,
                                parallelLevel: Int = 1)
                               (implicit
                                executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    try {
      if (check(x)) Future.successful(marker :: Nil)
      else Future.successful(Nil)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  override def firstSuccess(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[Option[MARKER]] = {
    try {
      if (check(x)) Future.successful(Some(marker))
      else Future.successful(None)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }
}


case class AsyncCheckNode[T, MARKER](marker: MARKER,
                                     check: T => Future[Boolean]) extends RulesNode[T, MARKER] {

  override def contramap[B](f: B => T): AsyncCheckNode[B, MARKER] = this.copy(check = f andThen this.check)

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): AsyncCheckNode[T, NEW_MARKER] = {
    this.copy(marker = f(marker))
  }

  override def collectFails(x: T,
                            parallelLevel: Int = 1)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    check(x).map { r =>
      if (!r) marker :: Nil
      else Nil
    }(FutureUtils.sameThreadExecutionContext)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    check(x).map { r =>
      if (!r) Some(marker)
      else None
    }(FutureUtils.sameThreadExecutionContext)
  }
  override def collectSuccesses(x: T,
                                parallelLevel: Int = 1)
                               (implicit
                                executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {


    check(x).map { r =>
      if (r) marker :: Nil
      else Nil
    }(FutureUtils.sameThreadExecutionContext)
  }

  override def firstSuccess(x: T)
                           (implicit
                            executionContext: ExecutionContext): Future[Option[MARKER]] = {
    check(x).map { r =>
      if (r) Some(marker)
      else None
    }(FutureUtils.sameThreadExecutionContext)
  }
}

case class EngineNode[T, MARKER](engine: RuleEngine[T,  MARKER]) extends RulesNode[T, MARKER] {

  override def contramap[B](f: B => T): EngineNode[B, MARKER] = EngineNode(engine.contramap(f))

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): RulesNode[T, NEW_MARKER] = {
    EngineNode(engine.mapMarkers(f))
  }

  override def collectFails(x: T,
                            parallelLevel: Int = 1)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    FutureUtils.batched(engine.validationsList(), parallelLevel)(_.collectFails(x))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    def recursiveSearch(checks: immutable.Seq[RulesNode[T, MARKER]]): Future[Option[MARKER]] = {
      checks match {
        case head +: tail =>
          head.firstFail(x).flatMap {
            case res@Some(_) => Future.successful(res)
            case _ => recursiveSearch(tail)
          }(FutureUtils.sameThreadExecutionContext)
        case _ => Future.successful(None)
      }
    }

    try {
      recursiveSearch(engine.validationsList())
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }
  override def collectSuccesses(x: T,
                                parallelLevel: Int = 1)
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    FutureUtils.batched(engine.validationsList(), parallelLevel)(_.collectSuccesses(x))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  override def firstSuccess(x: T)
                           (implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {
    def recursiveSearch(checks: immutable.Seq[RulesNode[T, MARKER]]): Future[Option[MARKER]] = {
      checks match {
        case v +: tail =>
          v.firstSuccess(x).flatMap {
            case res@Some(_) => Future.successful(res)
            case _ => recursiveSearch(tail)
          }(FutureUtils.sameThreadExecutionContext)
        case _ => Future.successful(None)
      }
    }

    try {
      recursiveSearch(engine.validationsList())
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }
}

case class BuildingEngineNode[T, MARKER](build: T => RuleEngine[T, MARKER]) extends RulesNode[T, MARKER] {

  override def contramap[B](f: B => T): BuildingEngineNode[B, MARKER] = {
    BuildingEngineNode { x =>
      build(f(x)).contramap(f)
    }
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): RulesNode[T, NEW_MARKER] = {
    BuildingEngineNode { x =>
      build(x).mapMarkers(f)
    }
  }

  override def collectFails(x: T,
                            parallelLevel: Int = 1)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    build(x).collectFails(x, parallelLevel)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {
    build(x).firstFail(x)
  }
  override def collectSuccesses(x: T,
                                parallelLevel: Int = 1)
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    build(x).collectSuccesses(x, parallelLevel)
  }

  override def firstSuccess(x: T)(implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {

    build(x).firstSuccess(x)
  }

}

case class SeqEngineNode[T, ENTRY, MARKER](engine: RuleEngine[ENTRY, MARKER],
                                           readSeq: T => immutable.Seq[ENTRY]) extends RulesNode[T, MARKER] {

  override def contramap[B](f: B => T) = {
    SeqEngineNode(engine, f andThen readSeq)
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): SeqEngineNode[T, ENTRY, NEW_MARKER] = {
    SeqEngineNode(engine.mapMarkers(f), readSeq)
  }

  override def collectFails(x: T,
                            parallelLevel: Int = 1)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    FutureUtils.serially(readSeq(x))(engine.collectFails(_, parallelLevel))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recursieveSearch(xs: Seq[ENTRY]): Future[Option[MARKER]] = xs match {
      case head +: tail =>
        engine.firstFail(head).flatMap {
          case s@Some(_) => Future.successful(s)
          case _ => recursieveSearch(tail)
        }(FutureUtils.sameThreadExecutionContext)
      case _ => Future.successful(None)
    }

    val seq = readSeq(x)
    recursieveSearch(seq)
  }

  override def collectSuccesses(x: T,
                                parallelLevel: Int = 1)
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {
    FutureUtils.serially(readSeq(x))(engine.collectSuccesses(_, parallelLevel))
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

case class OptEngineNode[T, ENTRY, MARKER](engine: RuleEngine[ENTRY, MARKER],
                                           readOpt: T => Option[ENTRY]) extends RulesNode[T, MARKER] {

  override def contramap[B](f: B => T): OptEngineNode[B, ENTRY, MARKER] = {
    OptEngineNode(engine, f.andThen(readOpt))
  }

  override def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): OptEngineNode[T, ENTRY, NEW_MARKER] = {
    OptEngineNode(engine.mapMarkers(f), readOpt)
  }

  override def collectFails(in: T,
                            parallelLevel: Int = 1)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    readOpt(in) match {
      case Some(value) => engine.collectFails(value, parallelLevel)
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
  override def collectSuccesses(in: T,
                                parallelLevel: Int = 1)
                               (implicit
                                executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {


    readOpt(in) match {
      case Some(value) => engine.collectSuccesses(value, parallelLevel)
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

