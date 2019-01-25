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
    new RuleEngine[INPUT, OUT] {}
  }

  def from[INPUT, OUT](seq: Seq[RulesNode[INPUT, OUT]]): RuleEngine[INPUT, OUT] = {

    new RuleEngine[INPUT, OUT] {
      this.nodes ++= seq

    }

  }
}


trait RuleEngine[INPUT, OUT] {
  self =>


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

  // nodes

  def validationsList(): immutable.Seq[RulesNode[INPUT, OUT]] = nodes.result()



  def contramap[B](f: B => INPUT): RuleEngine[B, OUT] = {
    RuleEngine.from(validationsList().map(_.contramap(f)))
  }

  def mapMarkers[NEW_MARKER](f: OUT => NEW_MARKER): RuleEngine[INPUT, NEW_MARKER] = {
    RuleEngine.from(validationsList().map(_.mapMarkers(f)))
  }

  def ++(other: RuleEngine[INPUT, OUT]): RuleEngine[INPUT, OUT] = {
    RuleEngine.from(this.validationsList() ++ other.validationsList())
  }

  protected def ruleAsync(marker: OUT)
                         (block: INPUT => Future[Boolean]): Unit = {
    nodes += AsyncCheckNode(marker, block)
  }

  protected def rule(marker: OUT)
                    (block: INPUT => Boolean): Unit = {
    nodes += SyncCheckNode(marker, block)
  }

  protected def register(v: RuleEngine[INPUT, OUT]): Unit = {
    nodes += EngineNode(v)
  }

  protected def registerOnSeq[B](extract: INPUT => immutable.Seq[B])
                                (v: RuleEngine[B, OUT]): Unit = {
    nodes += SeqEngineNode[INPUT, B, OUT](v, extract)
  }

  protected def registerOnOpt[B](extract: INPUT => Option[B])
                                (v: RuleEngine[B, OUT]): Unit = {
    nodes += OptEngineNode[INPUT, B, OUT](v, extract)
  }


  protected def registerOnFunc[B](build: INPUT => RuleEngine[INPUT, OUT]): Unit = {
    nodes += BuildingEngineNode[INPUT, OUT](build)
  }

  protected def registerOnMapping[KEY](extract: INPUT => KEY)
                                      (builders: (KEY, RuleEngine[INPUT, OUT])*): Unit = {
    val mapping = builders.groupBy { case (k, _) => k }
      .mapValues(validators => validators.map { case (_, v) => v }.reduce(_ ++ _))
      .map(identity) // remove laziness

    val empty = RuleEngine.empty[INPUT, OUT]

    registerOnFunc { obj =>
      val input = extract(obj)
      mapping.getOrElse(input, empty)
    }
  }

  protected def registerPartial[B](extract: INPUT => B)
                                  (builder: PartialFunction[B, RuleEngine[INPUT, OUT]]): Unit = {
    val empty = RuleEngine.empty[INPUT, OUT]
    registerOnFunc { obj =>
      val input = extract(obj)
      builder.applyOrElse(input, (_: B) => empty)
    }
  }

  protected def registerIf[B](test: INPUT => Boolean)
                             (engine: RuleEngine[INPUT, OUT]): Unit = {
    registerPartial(identity) {
      case obj if test(obj) => engine
    }
  }


  protected val nodes = new VectorBuilder[RulesNode[INPUT, OUT]]()
}
