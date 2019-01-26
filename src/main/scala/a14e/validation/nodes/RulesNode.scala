package a14e.validation.nodes

import a14e.validation.engines.RuleEngine
import a14e.validation.utils.FutureUtils

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.control.NonFatal


trait RulesNode[T, OUT] {

  def collectFails(x: T,
                   parallelLevel: Int = 1)
                  (implicit
                   executionContext: ExecutionContext): Future[immutable.Seq[OUT]]

  def firstFail(x: T)
               (implicit
                executionContext: ExecutionContext): Future[Option[OUT]]


  def collectSuccesses(x: T,
                       parallelLevel: Int = 1)
                      (implicit
                       executionContext: ExecutionContext): Future[immutable.Seq[OUT]]

  def firstSuccess(x: T)
                  (implicit
                   executionContext: ExecutionContext): Future[Option[OUT]]
}


object RulesNode {
  def merge[IN, OUT](nodes: immutable.Seq[RulesNode[IN, OUT]]): RulesNode[IN, OUT] = {
    nodes match {
      case Seq() => RuleEngine.empty[IN, IN, OUT, OUT](identity, identity)
      case Seq(x) => x
      case _ =>

        RuleEngine.from[IN, IN, OUT, OUT](nodes, identity, identity)
    }
  }
}


case class SyncCheckNode[IN, OUT](out: OUT,
                                  check: IN => Boolean) extends RulesNode[IN, OUT] {
  override def collectFails(x: IN,
                            parallelLevel: Int = 1)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {

    try {
      if (!check(x)) Future.successful(out :: Nil)
      else Future.successful(Nil)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  override def firstFail(x: IN)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[OUT]] = {
    try {
      if (!check(x)) Future.successful(Some(out))
      else Future.successful(None)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  override def collectSuccesses(x: IN,
                                parallelLevel: Int = 1)
                               (implicit
                                executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {

    try {
      if (check(x)) Future.successful(out :: Nil)
      else Future.successful(Nil)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  override def firstSuccess(x: IN)
                           (implicit
                            executionContext: ExecutionContext): Future[Option[OUT]] = {
    try {
      if (check(x)) Future.successful(Some(out))
      else Future.successful(None)
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }
}


case class AsyncCheckNode[IN, OUT](out: OUT,
                                  check: IN => Future[Boolean]) extends RulesNode[IN, OUT] {

  override def collectFails(x: IN,
                            parallelLevel: Int = 1)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {

    check(x).map { r =>
      if (!r) out :: Nil
      else Nil
    }(FutureUtils.sameThreadExecutionContext)
  }

  override def firstFail(x: IN)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[OUT]] = {
    check(x).map { r =>
      if (!r) Some(out)
      else None
    }(FutureUtils.sameThreadExecutionContext)
  }

  override def collectSuccesses(x: IN,
                                parallelLevel: Int = 1)
                               (implicit
                                executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {


    check(x).map { r =>
      if (r) out :: Nil
      else Nil
    }(FutureUtils.sameThreadExecutionContext)
  }

  override def firstSuccess(x: IN)
                           (implicit
                            executionContext: ExecutionContext): Future[Option[OUT]] = {
    check(x).map { r =>
      if (r) Some(out)
      else None
    }(FutureUtils.sameThreadExecutionContext)
  }
}


case class BuildingNode[T, OUT](build: T => RulesNode[T, OUT]) extends RulesNode[T, OUT] {

  override def collectFails(x: T,
                            parallelLevel: Int = 1)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    build(x).collectFails(x, parallelLevel)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[OUT]] = {
    build(x).firstFail(x)
  }

  override def collectSuccesses(x: T,
                                parallelLevel: Int = 1)
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    build(x).collectSuccesses(x, parallelLevel)
  }

  override def firstSuccess(x: T)(implicit executionContext: ExecutionContext): Future[Option[OUT]] = {

    build(x).firstSuccess(x)
  }

}

case class SeqNestedNode[T, ENTRY, OUT](engine: RulesNode[ENTRY, OUT],
                                        readSeq: T => immutable.Seq[ENTRY]) extends RulesNode[T, OUT] {

  override def collectFails(x: T,
                            parallelLevel: Int = 1)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {

    FutureUtils.serially(readSeq(x))(engine.collectFails(_, parallelLevel))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  override def firstFail(x: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[OUT]] = {

    def recursieveSearch(xs: Seq[ENTRY]): Future[Option[OUT]] = xs match {
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
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    FutureUtils.serially(readSeq(x))(engine.collectSuccesses(_, parallelLevel))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  override def firstSuccess(x: T)
                           (implicit executionContext: ExecutionContext): Future[Option[OUT]] = {

    def recFind(xs: Seq[ENTRY]): Future[Option[OUT]] = xs match {
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

case class OptNestedNode[T, ENTRY, OUT](engine: RulesNode[ENTRY, OUT],
                                        readOpt: T => Option[ENTRY]) extends RulesNode[T, OUT] {
  override def collectFails(in: T,
                            parallelLevel: Int = 1)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {

    readOpt(in) match {
      case Some(value) => engine.collectFails(value, parallelLevel)
      case None => Future.successful(Nil)
    }
  }

  override def firstFail(in: T)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[OUT]] = {
    readOpt(in) match {
      case Some(value) => engine.firstFail(value)
      case None => Future.successful(None)
    }
  }

  override def collectSuccesses(in: T,
                                parallelLevel: Int = 1)
                               (implicit
                                executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {


    readOpt(in) match {
      case Some(value) => engine.collectSuccesses(value, parallelLevel)
      case None => Future.successful(Nil)
    }
  }

  override def firstSuccess(x: T)(implicit executionContext: ExecutionContext): Future[Option[OUT]] = {


    readOpt(x) match {
      case Some(value) => engine.firstSuccess(value)
      case None => Future.successful(None)
    }
  }
}

