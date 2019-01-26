package a14e.validation.engines

import a14e.validation.nodes._
import a14e.validation.utils.FutureUtils

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.control.NonFatal


object RuleEngine {
  def empty[IN, RULES_IN, RULES_OUT, OUT](preprocess: IN => RULES_IN,
                                          postprocess: RULES_OUT => OUT): RuleEngine[IN, RULES_IN, RULES_OUT, OUT] = {
    new EmptyRulesEngine[IN, RULES_IN, RULES_OUT, OUT](preprocess, postprocess)
  }

  def from[IN, RULES_IN, RULES_OUT, OUT](seq: immutable.Seq[RulesNode[RULES_IN, RULES_OUT]],
                                         preprocess: IN => RULES_IN,
                                         postprocess: RULES_OUT => OUT): RuleEngine[IN, RULES_IN, RULES_OUT, OUT] = {
    new ImmutableRulesEngine(seq, preprocess, postprocess)
  }
}


trait RuleEngine[IN, RULES_IN, RULES_OUT, OUT] extends RulesNode[IN, OUT] {
  self =>

  // nodes

  def prepocess(in: IN): RULES_IN

  def postprocess(out: RULES_OUT): OUT


  def rules(): immutable.Seq[RulesNode[RULES_IN, RULES_OUT]]

  def toImmutable: RuleEngine[IN, RULES_IN, RULES_OUT, OUT] = RuleEngine.from(rules(), prepocess, postprocess)

  def ++(other: RuleEngine[IN, RULES_IN, RULES_OUT, OUT]): RuleEngine[IN, RULES_IN, RULES_OUT, OUT] = {
    RuleEngine.from(this.rules() ++ other.rules(), prepocess, postprocess)
  }

  // TODO dont rebuild everything on contramap

  def contramap[B](f: B => IN): RuleEngine[B, RULES_IN, RULES_OUT, OUT] = {
    RuleEngine.from[B, RULES_IN, RULES_OUT, OUT](rules(), f andThen prepocess, postprocess)
  }

  def map[NEW_OUT](f: OUT => NEW_OUT): RuleEngine[IN, RULES_IN, RULES_OUT, NEW_OUT] = {
    RuleEngine.from[IN, RULES_IN, RULES_OUT, NEW_OUT](rules(), prepocess, x => f(postprocess(x)))
  }

  def firstFail(x: IN)
               (implicit
                executionContext: ExecutionContext): Future[Option[OUT]] = {
    def recursiveSearch(prepared: RULES_IN,
                        checks: immutable.Seq[RulesNode[RULES_IN, RULES_OUT]]): Future[Option[OUT]] = {
      checks match {
        case head +: tail =>
          head.firstFail(prepared).flatMap {
            case Some(res) =>
              val completedRes = postprocess(res)
              Future.successful(Some(completedRes))
            case _ => recursiveSearch(prepared, tail)
          }(FutureUtils.sameThreadExecutionContext)
        case _ => Future.successful(None)
      }
    }

    try {
      val prepared = prepocess(x)
      recursiveSearch(prepared, rules())
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  def collectFails(x: IN,
                   parallelLevel: Int = 1)
                  (implicit
                   executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    val prepared = prepocess(x)
    FutureUtils.batched(rules(), parallelLevel)(_.collectFails(prepared))
      .map(_.flatten.map(postprocess))(FutureUtils.sameThreadExecutionContext)
  }


  def collectSuccesses(x: IN,
                       parallelLevel: Int = 1)
                      (implicit executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    val prepared = prepocess(x)
    FutureUtils.batched(rules(), parallelLevel)(_.collectSuccesses(prepared))
      .map(_.flatten.map(postprocess))(FutureUtils.sameThreadExecutionContext)
  }

  def firstSuccess(x: IN)
                  (implicit executionContext: ExecutionContext): Future[Option[OUT]] = {
    def recursiveSearch(prepared: RULES_IN,
                        checks: immutable.Seq[RulesNode[RULES_IN, RULES_OUT]]): Future[Option[OUT]] = {
      checks match {
        case head +: tail =>
          head.firstSuccess(prepared).flatMap {
            case Some(res) =>
              val completedRes = postprocess(res)
              Future.successful(Some(completedRes))
            case _ => recursiveSearch(prepared, tail)
          }(FutureUtils.sameThreadExecutionContext)
        case _ => Future.successful(None)
      }
    }

    try {
      val prepared = prepocess(x)
      recursiveSearch(prepared, rules())
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
