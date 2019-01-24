package a14e.validation

import a14e.validation.containers.{AsyncCheckLeaf, AsyncRulesCheck, BuildingEngineLeaf, OptEngineLeaf, SeqEngineLeaf, SyncCheckLeaf, SyncRulesCheck, RulesLeaf, EngineLeaf}
import a14e.validation.results.TextResult
import a14e.validation.utils.FutureUtils

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Random, Try}
import scala.util.control.NonFatal


object RuleEngine {
  def empty[INPUT, MARKER]: RuleEngine[INPUT, MARKER] = {
    new RuleEngine[INPUT, MARKER] {}
  }

  def from[INPUT, MARKER](seq: TraversableOnce[RulesLeaf[INPUT, MARKER]]): RuleEngine[INPUT, MARKER] = {

    new RuleEngine[INPUT, MARKER] {
      this.leafs ++= seq

    }

  }
}


trait RuleEngine[INPUT, MARKER] {
  self =>


  def firstFail(x: INPUT)
               (implicit
                executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recursiveSearch(checks: immutable.Seq[RulesLeaf[INPUT, MARKER]]): Future[Option[MARKER]] = {
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
      recursiveSearch(validationsList())
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  def firstFailWith(x: INPUT)
                   (markersToError: Option[MARKER] => Option[Throwable])
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
                      (markersToError: Option[MARKER] => Throwable)
                      (implicit
                       executionContext: ExecutionContext): Future[Unit] = {
    firstFailWith(x)(res => Some(markersToError(res)))
  }

  def failWith(x: INPUT,
               parallelLevel: Int = 1)
              (markersToError: immutable.Seq[MARKER] => Option[Throwable])
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
                 (markersToError: immutable.Seq[MARKER] => Throwable)
                 (implicit
                  executionContext: ExecutionContext): Future[Unit] = {
    failWith(x, parallelLevel)(res => Some(markersToError(res)))
  }

  def collectFails(x: INPUT,
                   parallelLevel: Int = 1)
                  (implicit
                   executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    FutureUtils.batched(validationsList(), parallelLevel)(_.collectFails(x))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }


  def collectSuccesses(x: INPUT,
                       parallelLevel: Int = 1)
                      (implicit executionContext: ExecutionContext): Future[immutable.Seq[MARKER]] = {

    FutureUtils.batched(validationsList(), parallelLevel)(_.collectSuccesses(x))
      .map(_.flatten)(FutureUtils.sameThreadExecutionContext)
  }

  def firstSuccess(x: INPUT)
                  (implicit executionContext: ExecutionContext): Future[Option[MARKER]] = {

    def recursiveSearch(checks: immutable.Seq[RulesLeaf[INPUT, MARKER]]): Future[Option[MARKER]] = {
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
      recursiveSearch(validationsList())
    } catch {
      case NonFatal(e) => Future.failed(e)
    }
  }

  // nodes

  def validationsList(): immutable.Seq[RulesLeaf[INPUT, MARKER]] = leafs.toList



  def contramap[B](f: B => INPUT): RuleEngine[B, MARKER] = {
    RuleEngine.from(leafs.map(_.contramap(f)))
  }

  def mapMarkers[NEW_MARKER](f: MARKER => NEW_MARKER): RuleEngine[INPUT, NEW_MARKER] = {
    RuleEngine.from(leafs.map(_.mapMarkers(f)))
  }

  def ++(other: RuleEngine[INPUT, MARKER]): RuleEngine[INPUT, MARKER] = {
    RuleEngine.from(this.leafs ++ other.leafs)
  }

  protected def ruleAsync(marker: MARKER)
                         (block: INPUT => Future[Boolean]): Unit = {
    leafs += AsyncCheckLeaf(AsyncRulesCheck(marker, block))
  }

  protected def rule(marker: MARKER)
                    (block: INPUT => Boolean): Unit = {
    leafs += SyncCheckLeaf(SyncRulesCheck(marker, block))
  }

  protected def register(v: RuleEngine[INPUT, MARKER]): Unit = {
    leafs += EngineLeaf(v)
  }

  protected def registerOnSeq[B](extract: INPUT => immutable.Seq[B])
                                (v: RuleEngine[B, MARKER]): Unit = {
    leafs += SeqEngineLeaf[INPUT, B, MARKER](v, extract)
  }

  protected def registerOnOpt[B](extract: INPUT => Option[B])
                                (v: RuleEngine[B, MARKER]): Unit = {
    leafs += OptEngineLeaf[INPUT, B, MARKER](v, extract)
  }


  protected def registerOnFunc[B](build: INPUT => RuleEngine[INPUT, MARKER]): Unit = {
    leafs += BuildingEngineLeaf[INPUT, MARKER](build)
  }

  protected def registerOnMapping[KEY](extract: INPUT => KEY)
                                      (builders: (KEY, RuleEngine[INPUT, MARKER])*): Unit = {
    val mapping = builders.groupBy { case (k, _) => k }
      .mapValues(validators => validators.map { case (_, v) => v }.reduce(_ ++ _))
      .map(identity) // remove laziness

    val empty = RuleEngine.empty[INPUT, MARKER]

    registerOnFunc { obj =>
      val input = extract(obj)
      mapping.getOrElse(input, empty)
    }
  }

  protected def registerPartial[B](extract: INPUT => B)
                                  (builder: PartialFunction[B, RuleEngine[INPUT, MARKER]]): Unit = {
    val empty = RuleEngine.empty[INPUT, MARKER]
    registerOnFunc { obj =>
      val input = extract(obj)
      builder.applyOrElse(input, (_: B) => empty)
    }
  }

  protected def registerIf[B](test: INPUT => Boolean)
                             (engine: RuleEngine[INPUT, MARKER]): Unit = {
    registerPartial(identity) {
      case obj if test(obj) => engine
    }
  }


  protected val leafs = new ArrayBuffer[RulesLeaf[INPUT, MARKER]]()
}
