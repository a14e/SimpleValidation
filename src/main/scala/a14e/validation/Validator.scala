package a14e.validation

import a14e.validation.engines.MutableRulesEngine
import a14e.validation.utils.FutureUtils

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}


trait Validator[IN, OUT] extends MutableRulesEngine[IN, OUT] {


}