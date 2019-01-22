package a14e.utils

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.mockito.MockitoSugar
import org.scalatest.time.{Millis, Seconds, Span}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration


trait DefaultSpec
  extends FlatSpec
    with ScalaFutures
    with Matchers
    with MockitoSugar {
  self =>

  trait ConcurrentWirings {
    implicit def excecutionContext: ExecutionContext = ExecutionContext.global
  }

  protected implicit val overridenPatienceConfig = PatienceConfig(Span(120, Seconds), Span(15, Millis))

}