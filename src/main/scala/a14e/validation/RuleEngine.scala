package a14e.validation

import a14e.validation.containers.{AsyncCheckNode, BuildingEngineNode, EngineNode, OptEngineNode, RulesNode, SeqEngineNode, SyncCheckNode}
import a14e.validation.results.TextResult
import a14e.validation.utils.FutureUtils

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Random, Try}
import scala.util.control.NonFatal


object RuleEngine {
  def empty[INPUT, OUT]: RuleEngine[INPUT, OUT] = {
    new EmptyRulesEngine[INPUT, OUT]
  }

  def from[INPUT, OUT](seq: immutable.Seq[RulesNode[INPUT, OUT]]): RuleEngine[INPUT, OUT] = {
    new ImmutableRulesEngine(seq)
  }
}


trait RuleEngine[INPUT, OUT] {
  self =>

  // nodes

  def rules(): immutable.Seq[RulesNode[INPUT, OUT]]

  def toImmutable: RuleEngine[INPUT, OUT] = RuleEngine.from(rules())

  def ++(other: RuleEngine[INPUT, OUT]): RuleEngine[INPUT, OUT] = {
    RuleEngine.from(this.rules() ++ other.rules())
  }

  def contramap[B](f: B => INPUT): RuleEngine[B, OUT] = {
    RuleEngine.from(rules().map(_.contramap(f)))
  }

  def mapMarkers[NEW_MARKER](f: OUT => NEW_MARKER): RuleEngine[INPUT, NEW_MARKER] = {
    RuleEngine.from(rules().map(_.mapMarkers(f)))
  }

  def firstFail(x: INPUT)
               (implicit
                executionContext: ExecutionContext): Future[Option[OUT]] = {
    EngineNode(this).firstFail(x)
  }

  def collectFails(x: INPUT,
                   parallelLevel: Int = 1)
                  (implicit
                   executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    EngineNode(this).collectFails(x, parallelLevel)
  }


  def collectSuccesses(x: INPUT,
                       parallelLevel: Int = 1)
                      (implicit executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    EngineNode(this).collectSuccesses(x, parallelLevel)
  }

  def firstSuccess(x: INPUT)
                  (implicit executionContext: ExecutionContext): Future[Option[OUT]] = {
    EngineNode(this).firstSuccess(x)
  }

  def firstFailWith(x: INPUT)
                   (markersToError: Option[OUT] => Option[Throwable])
                   (implicit
                    executionContext: ExecutionContext): Future[Unit] = {
    firstFail(x).flatMap { markers =>
      markersToError(markers) match {
        case Some(error) => Future.failed(error)
        case None => Future.unit
      }
    }(FutureUtils.sameThreadExecutionContext)
  }

  def firstFailWithErr(x: INPUT)
                      (markersToError: Option[OUT] => Throwable)
                      (implicit
                       executionContext: ExecutionContext): Future[Unit] = {
    firstFailWith(x)(res => Some(markersToError(res)))
  }

  def failWith(x: INPUT,
               parallelLevel: Int = 1)
              (markersToError: immutable.Seq[OUT] => Option[Throwable])
              (implicit
               executionContext: ExecutionContext): Future[Unit] = {
    collectFails(x, parallelLevel)
      .flatMap { markers =>
        markersToError(markers) match {
          case Some(error) => Future.failed(error)
          case None => Future.unit
        }
      }(FutureUtils.sameThreadExecutionContext)
  }

  def failWithErr(x: INPUT,
                  parallelLevel: Int = 1)
                 (markersToError: immutable.Seq[OUT] => Throwable)
                 (implicit
                  executionContext: ExecutionContext): Future[Unit] = {
    failWith(x, parallelLevel)(res => Some(markersToError(res)))
  }
}
