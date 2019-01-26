package a14e.validation.engines

import a14e.validation.nodes._

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}


class ImmutableRulesEngine[IN, RULES_IN, RULES_OUT, OUT](seq: immutable.Seq[RulesNode[RULES_IN, RULES_OUT]],
                                                         preprocessTask: IN => RULES_IN,
                                                         postprocessTask: RULES_OUT => OUT)
  extends RuleEngine[IN, RULES_IN, RULES_OUT, OUT] {

  override def rules(): immutable.Seq[RulesNode[RULES_IN, RULES_OUT]] = seq

  override def prepocess(in: IN): RULES_IN = preprocessTask(in)

  override def postprocess(out: RULES_OUT): OUT = postprocessTask(out)
}

class EmptyRulesEngine[IN, RULES_IN, RULES_OUT, OUT](preprocess: IN => RULES_IN,
                                                     postprocess: RULES_OUT => OUT)
  extends ImmutableRulesEngine[IN, RULES_IN, RULES_OUT, OUT](Nil, preprocess, postprocess) {

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

}

trait MutableRulesEngine[INPUT, OUT] extends RuleEngine[INPUT, INPUT, OUT, OUT] {

  override def prepocess(in: INPUT): INPUT = in

  override def postprocess(out: OUT): OUT = out

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

  protected def registerOnMapping[KEY](extract: INPUT => KEY)
                                      (pairs: (KEY, RulesNode[INPUT, OUT])*): Unit = {
    val mapping = pairs.groupBy { case (k, _) => k }
      .mapValues { nodesWithKeys =>
        val nodes = nodesWithKeys.collect { case (_, n) => n }.toList
        RulesNode.merge(nodes)
      }
      .map(identity) // remove laziness

    val empty = buildEmpty()

    registerOnFunc { obj =>
      val input = extract(obj)
      mapping.getOrElse(input, empty)
    }
  }

  protected def registerPartial[B](extract: INPUT => B)
                                  (builder: PartialFunction[B, RulesNode[INPUT, OUT]]): Unit = {
    val empty = buildEmpty()
    registerOnFunc { obj =>
      val input = extract(obj)
      builder.applyOrElse(input, (_: B) => empty)
    }
  }

  protected def registerIf[B](test: INPUT => Boolean)
                             (engine: RulesNode[INPUT, OUT]): Unit = {
    val empty = buildEmpty()
    registerOnFunc { obj =>
      if (test(obj)) engine
      else empty
    }
  }

  private def buildEmpty() = RuleEngine.empty[INPUT, INPUT, OUT, OUT](prepocess, postprocess)

  protected val nodes = new ListBuffer[RulesNode[INPUT, OUT]]()
}
