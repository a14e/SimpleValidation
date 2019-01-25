package a14e.validation

import a14e.validation.containers._

import scala.collection.immutable
import scala.collection.immutable.VectorBuilder
import scala.concurrent.{ExecutionContext, Future}


class ImmutableRulesEngine[INPUT, OUT](seq: immutable.Seq[RulesNode[INPUT, OUT]]) extends RuleEngine[INPUT, OUT] {
  override def rules(): immutable.Seq[RulesNode[INPUT, OUT]] = seq
}

class EmptyRulesEngine[INPUT, OUT] extends ImmutableRulesEngine[INPUT, OUT](Nil) {
  override def firstFail(x: INPUT)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[OUT]] = {
    Future.successful(None)
  }

  override def collectFails(x: INPUT, parallelLevel: Int)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    Future.successful(Nil)
  }

  override def firstSuccess(x: INPUT)
                           (implicit executionContext: ExecutionContext): Future[Option[OUT]] = {
    Future.successful(None)
  }


  override def collectSuccesses(x: INPUT, parallelLevel: Int)
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    Future.successful(Nil)
  }

}

trait MutableRulesEngine[INPUT, OUT] extends RuleEngine[INPUT, OUT] {

  def rules(): immutable.Seq[RulesNode[INPUT, OUT]] = nodes.result()

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
    nodes += SeqEngineNode(v, extract)
  }

  protected def registerOnOpt[B](extract: INPUT => Option[B])
                                (v: RuleEngine[B, OUT]): Unit = {
    nodes += OptEngineNode(v, extract)
  }

  protected def registerOnFunc[B](build: INPUT => RuleEngine[INPUT, OUT]): Unit = {
    nodes += BuildingEngineNode(build)
  }

  protected def registerOnMapping[KEY](extract: INPUT => KEY)
                                      (pairs: (KEY, RuleEngine[INPUT, OUT])*): Unit = {
    val mapping = pairs.groupBy { case (k, _) => k }
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
    val empty = RuleEngine.empty[INPUT, OUT]
    registerOnFunc { obj =>
      if (test(obj)) engine
      else empty
    }
  }

  protected val nodes = new VectorBuilder[RulesNode[INPUT, OUT]]()
}
