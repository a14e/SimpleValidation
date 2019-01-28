package a14e.validation.engines


import a14e.validation.nodes.{AsyncCheckNode, BuildingNode, OptNestedNode, RulesNode, SeqNestedNode, SyncCheckNode}

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds


class ImmutableRulesEngine[IN, OUT](seq: immutable.Seq[RulesNode[IN, OUT]]) extends RulesEngine[IN, OUT] {
  def rules(): immutable.Seq[RulesNode[IN, OUT]] = seq
}

class EmptyRulesEngine[IN, OUT] extends RulesEngine[IN, OUT] {

  override def firstFail(x: IN)
                        (implicit
                         executionContext: ExecutionContext): Future[Option[OUT]] = {
    Future.successful(None)
  }

  override def collectFails(x: IN, parallelLevel: Int)
                           (implicit
                            executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    Future.successful(Nil)
  }

  override def firstSuccess(x: IN)
                           (implicit executionContext: ExecutionContext): Future[Option[OUT]] = {
    Future.successful(None)
  }


  override def collectSuccesses(x: IN, parallelLevel: Int)
                               (implicit executionContext: ExecutionContext): Future[immutable.Seq[OUT]] = {
    Future.successful(Nil)
  }

  override def rules(): immutable.Seq[RulesNode[IN, OUT]] = Nil
}

trait MutableRulesEngine[INPUT, OUT] extends RulesEngine[INPUT, OUT] {


  def rules(): immutable.Seq[RulesNode[INPUT, OUT]] = nodes.result()

  protected def ruleAsync(marker: OUT)
                         (block: INPUT => Future[Boolean]): Unit = {
    nodes += AsyncCheckNode(marker, block)
  }

  protected def rule(marker: OUT)
                    (block: INPUT => Boolean): Unit = {
    nodes += SyncCheckNode(marker, block)
  }

  protected def register(v: RulesNode[INPUT, OUT]): Unit = {
    nodes += v
  }

  protected def registerOnSeq[ENTRY](extract: INPUT => immutable.Seq[ENTRY])
                                (v: RulesNode[ENTRY, OUT]): Unit = {
    nodes += SeqNestedNode(v, extract)
  }

  protected def registerOnOpt[ENTRY](extract: INPUT => Option[ENTRY])
                                (v: RulesNode[ENTRY, OUT]): Unit = {
    nodes += OptNestedNode(v, extract)
  }

  protected def registerOnFunc(build: INPUT => RulesNode[INPUT, OUT]): Unit = {
    nodes += BuildingNode(build)
  }

  protected def registerPartial[B](extract: INPUT => B)
                                  (builder: PartialFunction[B, RulesNode[INPUT, OUT]]): Unit = {
    val empty = RulesEngine.empty[INPUT, OUT]
    registerOnFunc { obj =>
      val input = extract(obj)
      builder.applyOrElse(input, (_: B) => empty)
    }
  }

  protected def registerIf[B](test: INPUT => Boolean)
                             (engine: RulesNode[INPUT, OUT]): Unit = {
    val empty =  RulesEngine.empty[INPUT, OUT]
    registerOnFunc { obj =>
      if (test(obj)) engine
      else empty
    }
  }

  protected val nodes = new ListBuffer[RulesNode[INPUT, OUT]]()

}