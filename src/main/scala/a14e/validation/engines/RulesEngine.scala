package a14e.validation.engines

import a14e.validation.nodes.RulesNode
import a14e.validation.utils.FutureUtils

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.control.NonFatal


object RulesEngine {
  def empty[IN, OUT]: RulesEngine[IN,  OUT] = {
    new EmptyRulesEngine()
  }

  def from[IN, OUT](seq: immutable.Seq[RulesNode[IN, OUT]]): RulesEngine[IN,  OUT] = {

    new ImmutableRulesEngine(seq)
  }
}


trait RulesEngine[IN, OUT] extends RulesNode[IN, OUT] {
  self =>

  // nodes

  def rules(): immutable.Seq[RulesNode[IN, OUT]]

  def toImmutable: RulesEngine[IN,  OUT] = RulesEngine.from(rules())

  def ++(other: RulesEngine[IN, OUT]): RulesEngine[IN, OUT] = {
    RulesEngine.from(this.rules() ++ other.rules())
  }

  def firstFail(x: IN)
               (implicit
                executionContext: ExecutionContext): Future[Option[OUT]] = {
    def recursiveSearch(prepared: IN,
                        checks: immutable.Seq[RulesNode[IN, OUT]]): Future[Option[OUT]] = {
      checks match {
        case head +: tail =>
          head.firstFail(prepared).flatMap {
            case Some(res) => Future.successful(Some(res))
            case _ => recursiveSearch(prepared, tail)
          }(FutureUtils.sameThreadExecutionContext)
        case _ => Future.successful(None)
      }
    }

    try {
      recursiveSearch(x, rules())
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  def collectFails(x: IN,
                   parallelLevel: Int = 1)
                  (implicit
                   executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    FutureUtils.batched(rules(), parallelLevel)(_.collectFails(x))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }


  def collectSuccesses(x: IN,
                       parallelLevel: Int = 1)
                      (implicit executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    FutureUtils.batched(rules(), parallelLevel)(_.collectSuccesses(x))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  def firstSuccess(x: IN)
                  (implicit executionContext: ExecutionContext): Future[Option[OUT]] = {
    def recursiveSearch(prepared: IN,
                        checks: immutable.Seq[RulesNode[IN, OUT]]): Future[Option[OUT]] = {
      checks match {
        case head +: tail =>
          head.firstSuccess(prepared).flatMap {
            case Some(res) => Future.successful(Some(res))
            case _ => recursiveSearch(prepared, tail)
          }(FutureUtils.sameThreadExecutionContext)
        case _ => Future.successful(None)
      }
    }

    try {
      recursiveSearch(x, rules())
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }


  def firstFailWith(x: IN)
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

  def firstFailWithErr(x: IN)
                      (markersToError: Option[OUT] => Throwable)
                      (implicit
                       executionContext: ExecutionContext): Future[Unit] = {
    firstFailWith(x)(res => Some(markersToError(res)))
  }

  def failWith(x: IN,
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

  def failWithErr(x: IN,
                  parallelLevel: Int = 1)
                 (markersToError: immutable.Seq[OUT] => Throwable)
                 (implicit
                  executionContext: ExecutionContext): Future[Unit] = {
    failWith(x, parallelLevel)(res => Some(markersToError(res)))
  }
}
